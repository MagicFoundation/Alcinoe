/// pascal implementation of BZ2 decompression
// - licensed under a MPL/GPL/LGPL tri-license; version 1.6
unit SynBzPas;

{
    This file is part of Synopse BZ2 Compression.

    Synopse Synopse BZ2 Compression. Copyright (C) 2021 Arnaud Bouchez
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

  The Original Code is Synopse BZ2 Compression.

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
}

{
      BZip2 decompression unit
      ========================

      Notes:
       - pascal and asm speed and memory optimization of the code
       - bug in main loop -> use it at once, TBZDecompressor is buggy
       - use UnCompressBzMem() global function

      HUGE WARNING / BIG DISCLOSURE / PLEASE README :
       - we discovered that this unit doesn't properly uncompress some big
         content, so you should rather not trust this unit - consider it
         as a proof-of-concept, not to be used on production

      initial release (c)2002 by Daniel Mantione
      modifications and speed-up by Arnaud Bouchez (c)2008 http://bouchez.info

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Classes,
  SysUtils;

{$ifndef FPC_OR_UNICODE}
type
  RawByteString = AnsiString;
{$endif}

function UnCompressBzMem(Source: pointer; SourceSize, DestSize: integer;
  error: PInteger = nil): TMemoryStream;
// result=nil if error decompressing

function UnCompressBzMemToString(Source: pointer; SourceSize, DestSize: integer): RawByteString;


implementation

const max_groups=6;
      max_alpha_size=258;
      max_code_len=23;
      group_size=50;
      iter_count=4;
      max_selectors=2+(900000 div group_size);

const mtfa_size=4096;
      mtfl_size=16;

type  TCardinal_array=array [0..899999] of cardinal;
      PCardinal_array=^Tcardinal_array;

      Thuffarray=array[0..max_alpha_size] of cardinal;
      Phuffarray=^Thuffarray;

      {$ifndef FPC}
      PtrInt = {$ifdef UNICODE}NativeInt{$else}integer{$endif};
      {$endif}

      PBZip2_Decode_Stream = ^TBZip2_Decode_Stream;
      TBZip2_Decode_Stream = object
      private
        bits_available: byte;
        read_data: byte;
        readstream: PByte;
        block_randomized: boolean;
        blocksize: cardinal;
        tt: PCardinal_Array;
        tt_count: cardinal;
        rle_run_left,rle_run_data: integer;
        nextrle: PByte;
        decode_available: cardinal;
        block_origin: cardinal;
        current_block: cardinal;
        inuse_count: cardinal;
        alphasize: cardinal;
        group_count,group_pos,gsel,gminlen: integer;
        group_no: cardinal;
        glimit,gperm,gbase: Phuffarray;
        selector_count: cardinal;
        seq_to_unseq: array[0..255] of byte;
        selector,selector_mtf: array[0..max_selectors] of byte;
        len: array[0..max_groups,0..max_alpha_size] of byte;
        limit: array[0..max_groups,0..max_alpha_size] of cardinal;
        base: array[0..max_groups,0..max_alpha_size] of cardinal;
        perm: array[0..max_groups,0..max_alpha_size] of cardinal;
        minlens: array[0..max_groups] of byte;
        cftab: array[0..257] of cardinal;
        mtfbase: array[0..256 div mtfl_size-1] of cardinal;
        mtfa: array[0..mtfa_size-1] of byte;
        function get_bits(n: cardinal): cardinal; {$ifdef HASINLINE}inline;{$endif}
        function get_boolean: boolean; {$ifdef HASINLINE}inline;{$endif}
        function get_cardinal24: cardinal;
        function get_cardinal: cardinal;
        procedure receive_mapping_table;
        procedure receive_selectors;
        procedure undo_mtf_values;
        procedure receive_coding_tables;
        procedure make_hufftab;
        procedure init_mtf;
        function get_mtf_value:cardinal;
        procedure move_mtf_block;
        procedure receive_mtf_values;
        procedure detransform;
        function decode_block:boolean;
        procedure rle_read(bufptr:PByte; var count:integer);
        procedure new_block;
        procedure consume_rle; {$ifdef HASINLINE}inline;{$endif}
      public
        error: integer;
        procedure Open(bz2source: pointer);
        procedure Read(bufptr: PByte; count:integer);
        procedure Close;
      end;

{Error codes for stream errorinfo.}
const BZip2_bad_header_magic        =1;
      BZip2_bad_block_magic         =2;
      BZip2_endoffile               =3;
      BZip2_data_error              =4;

procedure hb_create_decode_tables(limit, base, perm: PCardinal_Array;
  length: PByteArray; minlen, maxlen: byte; alphasize: cardinal);
var pp, i, j, vec: cardinal;
begin
  pp := 0;
  for i := minlen to maxlen do
    for j := 0 to alphasize-1 do
      if length[j]=i then begin
        perm[pp] := j;
        inc(pp);
      end;
  fillchar(base[0],(max_code_len-1)*4,0);
  fillchar(limit[0],(max_code_len-1)*4,0);
  for i := 0 to alphasize-1 do
    inc(base[length[i]+1]);
  for i := 1 to max_code_len-1 do
    inc(base[i],base[i-1]);
  vec := 0;
  for i := minlen to maxlen do begin
    inc(vec,base[i+1]-base[i]);
    limit[i] := vec-1;
    vec := vec shl 1;
  end;
  for i := minlen+1 to maxlen do
    base[i] := ((limit[i-1]+1) shl 1)-base[i];
end;

{*****************************************************************************
                             TBZip2_decode_stream
*****************************************************************************}

procedure TBZip2_decode_stream.Open(bz2source: pointer);
begin
  fillchar(self,256{sizeof(self)},0); // 256 is enough for reset
  error := 0;
  readstream := bz2source;
  {Read the magic.}
  if PCardinal(readstream)^ and $00ffffff<>ord('B')+ord('Z')shl 8+ord('h')shl 16 then begin
    error := BZip2_bad_header_magic;
    exit;
  end else
    inc(readstream,3);
  {Read the block size and allocate the working array.}
  blocksize:= (readstream^-ord('0'))*100000;
  inc(readstream);
  getmem(tt,blocksize*4);
  decode_available := high(decode_available);
end;

function TBZip2_decode_stream.get_cardinal24:cardinal;
begin
  get_cardinal24 := get_bits(8) shl 16 or get_bits(8) shl 8 or get_bits(8);
end;

function TBZip2_decode_stream.get_cardinal:cardinal;
begin
  get_cardinal := get_bits(8) shl 24 or get_bits(8) shl 16 or get_bits(8) shl 8 or get_bits(8);
end;

function TBZip2_decode_stream.get_bits(n: cardinal): cardinal;
var data,n8: cardinal;
begin
//  if n<>8 then begin // not faster :(
    if n<bits_available then begin
      result := read_data shr (8-n);
      read_data := read_data shl n;
      dec(bits_available,n);
    end
    else begin
      data := readstream^;
      inc(readstream);
      n8 := 8-n;
      result := (read_data shr n8) or (data shr (n8+bits_available));
      read_data := data shl (n-bits_available);
      inc(bits_available,n8);
    end;
{  end else begin
    data := readstream^; inc(readstream);
    result := read_data or (data shr bits_available);
    read_data := data shl (8-bits_available);
  end;}
end;

function TBZip2_decode_stream.get_boolean: boolean;
var data: cardinal;
begin
  if bits_available>0 then begin
    result := boolean(read_data shr 7);
    read_data := read_data shl 1;
    dec(bits_available);
  end
  else begin
    data := readstream^;
    inc(readstream);
    result := boolean(data shr 7);
    read_data := data shl 1;
    bits_available := 7;
  end;
end;

procedure TBZip2_decode_stream.receive_mapping_table;
{Receive the mapping table. To save space, the inuse set is stored in pieces
 of 16 bits. First 16 bits are stored which pieces of 16 bits are used, then
 the pieces follow.}
var i,j,v,inuse16: cardinal;
begin
  inuse16 := 0;
  {Receive the first 16 bits which tell which pieces are stored.}
  for i := 0 to 15 do
    if get_boolean then
      inuse16 := inuse16 or (1 shl i);
  {Receive the used pieces.}
  inuse_count := 0;
  v := 0;
  for i := 0 to 15 do begin
    if inuse16 or (1 shl i)<>0 then
      for j := 0 to 15 do
        if get_boolean then begin
          seq_to_unseq[inuse_count] := v+j;
          inc(inuse_count);
        end;
    inc(v,16);
  end;
end;

procedure TBZip2_decode_stream.receive_selectors;
{Receives the selectors.}
var data, i: cardinal;
    j: byte;
begin
  group_count := get_bits(3);
  selector_count:= (get_bits(8) shl 7) or get_bits(7);
  for i := 0 to selector_count-1 do begin
    j := 0;
// while get_boolean do begin
    repeat
      if bits_available<=0 then begin
        data := readstream^;
        inc(readstream);
        read_data := data shl 1;
        bits_available := 7;
        if data shr 7=0 then
          break;
        inc(j);
        if j<=5 then
          continue else begin
          error := BZip2_data_error;
          break;
        end;
      end
      else begin
        if read_data shr 7=0 then begin
          read_data := byte(read_data shl 1);
          dec(bits_available);
          break;
        end;
        read_data := byte(read_data shl 1);
        dec(bits_available);
        inc(j);
        if j<=5 then
          continue else begin
          error := BZip2_data_error;
          break;
        end;
      end;
    until false;
    selector_mtf[i] := j;
  end;
end;

procedure TBZip2_decode_stream.undo_mtf_values;
{Undo the MTF values for the selectors.}
var pos: array[0..max_groups] of byte;
    i, j: integer;
    v, tmp: byte;
begin
  for v := 0 to group_count-1 do
    pos[v] := v;
  for i :=0  to selector_count-1 do begin
      v := selector_mtf[i];
      tmp := pos[v];
      for j := v downto 1 do
        pos[j] := pos[j-1];
      pos[0] := tmp;
      selector[i] := tmp;
    end;
end;

procedure TBZip2_decode_stream.receive_coding_tables;
var t, curr: byte;
    i: cardinal;
begin
  for t := 0 to group_count-1 do begin
    curr := get_bits(5);
    for i := 0 to alphasize-1 do begin
      repeat
        if not(curr in [1..20]) then begin
          error := BZip2_data_error;
          exit;
        end;
        if not get_boolean then
          break;
        if get_boolean then
          dec(curr) else
          inc(curr);
      until false;
      len[t,i] := curr;
    end;
  end;
end;

procedure TBZip2_decode_stream.make_hufftab;
{Builds the Huffman tables.}
var i, t: cardinal;
    minlen, maxlen: byte;
begin
  for t := 0 to group_count-1 do begin
    minlen := 32;
    maxlen := 0;
    for i := 0 to alphasize-1 do begin
      if len[t,i]>maxlen then
        maxlen := len[t,i];
      if len[t,i]<minlen then
        minlen := len[t,i];
    end;
    hb_create_decode_tables(@limit[t],@base[t],@perm[t],@len[t],
                            minlen,maxlen,alphasize);
    minlens[t] := minlen;
  end;
end;

procedure TBZip2_decode_stream.init_mtf;
var i,j,v,k: cardinal;
begin
  k := mtfa_size-1;
  for i := 256 div mtfl_size-1 downto 0 do begin
    v := i*mtfl_size+mtfl_size-1;
    for j := mtfl_size-1 downto 0 do begin
      mtfa[k] := v;
      dec(k);
      dec(v);
    end;
    mtfbase[i] := k+1;
  end;
end;

function TBZip2_decode_stream.get_mtf_value: cardinal;
var data, zvec: cardinal;
    zn, n8: integer;
begin
  if group_pos=0 then begin
    inc(group_no);
    group_pos := group_size;
    gsel := selector[group_no];
    gminlen := minlens[gsel];
    glimit := @limit[gsel];
    gperm := @perm[gsel];
    gbase := @base[gsel];
  end;
  dec(group_pos);
  zn := gminlen;
//  zvec:=get_bits(zn);
  if zn>bits_available then begin
    data := readstream^;
    inc(readstream);
    n8 := 8-zn;
    zvec := (read_data shr n8) or (data shr (n8+bits_available));
    read_data := data shl (zn-bits_available);
    inc(bits_available,n8);
  end
  else begin
    zvec := read_data shr (8-zn);
    read_data:= read_data shl zn;
    dec(bits_available,zn);
  end;
  while zvec>glimit^[zn] do begin
    inc(zn);
//  zvec := (zvec shl 1) or cardinal(get_boolean);
    if bits_available<=0 then begin
      data := readstream^;
      inc(readstream);
      zvec := (zvec shl 1) or (data shr 7);
      read_data := data shl 1;
      bits_available := 7;
    end
    else begin
      zvec := (zvec shl 1) or (read_data shr 7);
      read_data := byte(read_data shl 1);
      dec(bits_available);
    end;
  end;
  get_mtf_value := gperm^[zvec-gbase^[zn]];
end;

procedure TBZip2_decode_stream.move_mtf_block;
var i: integer;
    k: cardinal;
begin
  k := MTFA_SIZE;
  for i := 256 div MTFL_SIZE-1 downto 0 do begin
    dec(k,16);
    move(mtfa[mtfbase[i]],mtfa[k],16);
    mtfbase[i] := k;
  end;
end;

function depl(p: PAnsiChar; n: PtrInt): cardinal; {$ifdef HASINLINE}inline;{$endif}
begin
  result := ord(p[n]);
  move(p[0],p[1],n);
  p[0] := char(result);
end;

function depl2(p: PAnsiChar; n: PtrInt): cardinal; {$ifdef HASINLINE}inline;{$endif}
begin
  result := ord(p[n]);
  move(p[0],p[1],n);
end;

procedure fill(p: PCardinal_array; n,v: integer); {$ifdef HASINLINE}inline;{$endif}
var i: integer;
begin
  for i := 0 to n-1 do
    p^[i] := v;
end;

procedure TBZip2_decode_stream.receive_mtf_values;
const run_a=0;
      run_b=1;
var t, next_sym: cardinal;
    es: cardinal;
    n, c, d: cardinal;
    nn, i: cardinal;
    u, v: PCardinal;
    lno, off: cardinal;
begin
  group_no := high(group_no);
  group_pos := 0;
  t := 0;
  fillchar(cftab,sizeof(cftab),0);
  init_mtf;
  next_sym := get_mtf_value;
  while next_sym<>inuse_count+1 do begin
    if next_sym<=run_b then begin
      es := 0;
      n := 0;
      repeat
        inc(es,(next_sym+1) shl n);
        inc(n);
        next_sym := get_mtf_value;
      until next_sym>run_b;
      n := seq_to_unseq[mtfa[mtfbase[0]]];
      inc(cftab[n],es);
      if t+es>blocksize then begin
        error := BZip2_data_error;
        exit;
      end;
      fill(@tt^[t],es,n);
      inc(t,es);
    end
    else
    begin
      nn := next_sym-1;
      if nn<mtfl_size then
        {Avoid the costs of the general case.}
        n := depl(@mtfa[mtfbase[0]],nn) else begin
        {General case.}
        lno := nn div MTFL_SIZE;
        off := nn and (MTFL_SIZE-1);
        n := depl2(@mtfa[mtfbase[lno]],off);
        u := @mtfbase;
        v := u;
        inc(v,lno);
        repeat
          mtfa[v^] := mtfa[PCardinal(PtrInt(v)-4)^+MTFL_SIZE-1];
          dec(v);
          dec(v^);
        until v=u;
        mtfa[v^] := n;
        if v^=0 then
          move_mtf_block;
      end;
      inc(cftab[seq_to_unseq[n]]);
      tt^[t] := seq_to_unseq[n];
      inc(t);
      if t>blocksize then begin
        error := BZip2_data_error;
        exit;
      end;
      next_sym := get_mtf_value;
    end;
  end;
  tt_count := t;
  {Setup cftab to facilitate generation of T^(-1).}
  c := 0;
  for i := 0 to 256 do begin
    d := cftab[i];
    cftab[i] := c;
    inc(c,d);
  end;
end;

procedure TBZip2_decode_stream.detransform;
{$ifdef PUREPASCAL}
var a: cardinal;
    c: PtrInt;
    p,pend: PByte;
    t,cf: PCardinal_array;
begin // efficient code on FPC x86-64
  if tt_count=0 then
    exit;
  t := @tt;
  p := pointer(t);
  pend := @t^[tt_count];
  cf := @cftab;
  a := 0;
  repeat
    c := p^;
    inc(p,4);
    t^[cf^[c]] := t^[cf^[c]] or a;
    inc(cf^[c]);
    inc(a,256);
  until p=pend;
end;
{$else} {$ifdef FPC} nostackframe; assembler; {$endif}
asm
      mov ecx,[eax+TBZip2_decode_stream.tt_count]
      jcxz @a2
      push ebx
      push ebp
      push esi
      push edi
      lea ebx,[eax+TBZip2_decode_stream.cftab]
      mov esi,[eax+TBZip2_decode_stream.tt]
      mov edi,esi
      xor edx,edx
@a1:  movzx eax,byte [esi]
      mov ebp,[ebx+4*eax]
      inc dword [ebx+4*eax]
      or [edi+ebp*4],edx
      add edx,$100
      add esi,4
      dec ecx
      jnz @a1
      pop edi
      pop esi
      pop ebp
      pop ebx
@a2:
end;
{$endif}

function TBZip2_decode_stream.decode_block: boolean;
{Decode a new compressed block.}
var magic: string[6];
//    stored_blockcrc:cardinal;
    i: integer;
begin
  result := false;
  magic[0] := #6;
  for i := 1 to 6 do
    magic[i] := char(get_bits(8));
  if magic='1AY&SY' then begin
    inc(current_block);
    {stored_blockcrc:=}get_cardinal;
    block_randomized := get_boolean;
    block_origin := get_cardinal24;
    {Receive the mapping table.}
    receive_mapping_table;
    alphasize := cardinal(inuse_count)+2;
    {Receive the selectors.}
    receive_selectors;
    if error<>0 then
      exit;
    {Undo the MTF values for the selectors.}
    undo_mtf_values;
    {Receive the coding tables.}
    receive_coding_tables;
    if error<>0 then
      exit;
    {Build the Huffman tables.}
    make_hufftab;
    {Receive the MTF values.}
    receive_mtf_values;
    {Undo the Burrows Wheeler transformation.}
    detransform;
    decode_available:=tt_count;
    result := true;
  end
  else
    if magic<>#$17'rE8P'#$90 then
      error := BZip2_bad_block_magic;
end;

procedure TBZip2_decode_stream.new_block;
begin
  if decode_block then
    nextrle := @tt^[tt^[block_origin] shr 8] else begin
    error := BZip2_endoffile;
    nextrle := nil;
  end;
end;

procedure TBZip2_decode_stream.consume_rle;
{Make nextrle point to the next decoded byte. If nextrle did point to the last
 byte in the current block, decode the next block.}
begin
  nextrle := @tt^[PCardinal(nextrle)^ shr 8];
  dec(decode_available);
  if decode_available=0 then
    new_block;
end;

procedure TBZip2_decode_stream.rle_read(bufptr:PByte;var count: integer);
var rle_len: integer;
    data: byte;
label rle_write;
begin
  if nextrle=nil then exit;
  rle_len := rle_run_left;
  data := rle_run_data;
  if block_randomized then
    {Not yet implemented.}
    runerror(212) else begin
    if rle_len<>0 then
      goto rle_write;
    repeat
      if decode_available=0 then
        break;
      rle_len := 1;
      data := nextrle^;
      nextrle := @tt^[PCardinal(nextrle)^ shr 8];
      dec(decode_available);
      if decode_available=0 then
        new_block;
      if (decode_available>0) and (data=nextrle^) then begin
        nextrle := @tt^[PCardinal(nextrle)^ shr 8];
        dec(decode_available);
        inc(rle_len);
        if decode_available=0 then
          new_block;
        if (decode_available>0) and (data=nextrle^) then begin
          inc(rle_len);
          consume_rle;
          if (decode_available>0) and (data=nextrle^) then begin
            consume_rle;
            inc(rle_len,nextrle^+1);
            consume_rle;
          end;
        end;
      end;
rle_write:
      if rle_len=1 then begin
        bufptr^ := data;
        dec(count);
        rle_len := 0;
        inc(bufptr);
        if count<>0 then
          continue else
          break;
      end else
      if count>=rle_len then begin
        fillchar(bufptr^,rle_len,data);
        dec(count,rle_len);
        inc(bufptr,rle_len);
        rle_len := 0;
      end else begin
        fillchar(bufptr^,count,data);
        dec(rle_len,count);
        count := 0;
        break; // count=0
      end;
    until false;
  end;
  rle_run_data := data;
  rle_run_left := rle_len;
end;

procedure TBZip2_decode_stream.read(bufptr: PByte; count:integer);
begin
  if decode_available=high(decode_available) then begin
    {Initialize the rle process:
      - Decode a block
      - Initialize pointer.}
    if not decode_block then begin
      error := BZip2_endoffile;
      nextrle := nil;
    end else
      nextrle := @tt^[tt^[block_origin] shr 8];
  end;
  rle_read(bufptr,count);
end;

procedure TBZip2_decode_stream.Close;
begin
  if tt<>nil then
    freemem(tt);
end;

(*
{ TBZDecompressor }

constructor TBZDecompressor.Create(Buffer: PByte);
begin
  GetMem(BZ,sizeof(TBZip2_decode_stream));
  PBZip2_Decode_Stream(BZ)^.Open(Buffer);
end;

destructor TBZDecompressor.Destroy;
begin
  if BZ<>nil then begin
    PBZip2_Decode_Stream(BZ)^.Close;
    FreeMem(BZ);
  end;
  inherited;
end;

function TBZDecompressor.Read(var Buffer; Count: Integer): Longint;
begin
  PBZip2_Decode_Stream(BZ)^.Read(Buffer,Count);
  Inc(DestLen,Count);
  Result := Count;
end;

function TBZDecompressor.Seek(Offset: Integer; Origin: Word): Longint;
begin
  result := DestLen;
  if (Offset<>0) or (Origin<>soFromCurrent) then  // for TStream.Position
    if Result<>0 then
      runerror(212);
end;

function TBZDecompressor.Write(const Buffer; Count: Integer): Longint;
begin
  runerror(212);
  Result := 0;
end;
*)

function UnCompressBzMem(Source: pointer; SourceSize, DestSize: integer;
  error: PInteger): TMemoryStream;
var BZ: PBZip2_decode_stream;
begin
  result := TMemoryStream.Create;
  if DestSize<=0 then exit;
  result.Size := DestSize;
  Getmem(BZ,sizeof(TBZip2_decode_stream));
  try
    BZ^.Open(Source);
    if BZ^.error=0 then
      BZ^.Read(result.Memory,DestSize);
    BZ^.Close;
    if BZ^.error<>BZip2_endoffile then
      FreeAndNil(result); // result=nil if error decompressing
    if error <> nil then
      error^ := BZ^.error;
  finally
    Freemem(BZ);
  end;
end;

function UnCompressBzMemToString(Source: pointer; SourceSize, DestSize: integer): RawByteString;
var BZ: PBZip2_decode_stream;
begin
  if DestSize<=0 then exit;
  SetLength(result,DestSize);
  Getmem(BZ,sizeof(TBZip2_decode_stream));
  try
    BZ^.Open(Source);
    if BZ^.error=0 then
      BZ^.Read(pointer(result),DestSize);
    BZ^.Close;
    if BZ^.error<>BZip2_endoffile then
      result := '';
  finally
    Freemem(BZ);
  end;
end;

end.
