//
// Single Png,Jpeg Reader for Pascal
//
//  http://blog.naver.com/simonsayz
//
//   History
//     2013.03.04 Started
//
//
//  This unit was tested on android, Nexus7
//
//  - Png Original Code : BeRoPNG.pas
//  - Jpg Original Code : NanoJpeg.pas
//
//
//
unit And_lib_Image;

{$ifdef fpc}
 {$mode delphi}
 {$warnings off}
 {$hints off}
 {$ifdef cpui386}
  {$define cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef fpc_little_endian}
  {$define little_endian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define big_endian}
  {$endif}
 {$endif}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
{$else}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu64}
 {$endif}
 {$optimization on}
 {$undef HasSAR}
 {$define UseDIV}
{$endif}
{$overflowchecks off}
{$rangechecks off}

interface

Uses
 SysUtils,Classes,And_jni_Bridge;

Type
 //
 TImgFormat   = (ifUnknown,
                 ifPng,
                 ifJpeg);
 //
 TPixFormat   = (pfUnKnown,
                 pfRGB,
                 pfRGBA);
 //
 TImgDecoder  = class
  private
   FImgFormat   : TImgFormat;
   FImgWidth    : Integer;
   FImgHeight   : Integer;
   FPixFormat   : TPixFormat;
   //
   FJpegDecoder : TObject;
   //
   Function  HasAlpha(ImgPtr : Pointer) : Boolean;
   Procedure SetAlpha(ImgPtr : Pointer; Alpha : Byte);
   //
   Function  GetImgFormat(Stream : TMemoryStream) : TImgFormat;
  public
   FImgPtr      : Pointer;
   //
   Constructor Create;  virtual;
   Destructor  Destroy; override;
   Procedure   Free;
   //
   Function LoadFromFile(FileName : String) : Boolean;
   //
   Property ImgWidth  : Integer    read FImgWidth;
   Property ImgHeight : Integer    read FImgHeight;
   Property ImgPtr    : Pointer    read FImgPtr;
   Property PixFormat : TPixFormat read FPixFormat;
  end;

implementation

// BeRoPNG;
(*************************************
** 2-clause simplified BSD license ***
**************************************
**
** Copyright 2010-2011 Benjamin Rosseaux. All rights reserved.
**
** Redistribution and use in source and binary forms, with or without modification, are
** permitted provided that the following conditions are met:
**
**    1. Redistributions of source code must retain the above copyright notice, this list of
**       conditions and the following disclaimer.
**
**    2. Redistributions in binary form must reproduce the above copyright notice, this list
**       of conditions and the following disclaimer in the documentation and/or other materials
**       provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY BENJAMIN ROSSEAUX ``AS IS'' AND ANY EXPRESS OR IMPLIED
** WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
** FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL BENJAMIN ROSSEAUX OR
** CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
** CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
** SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
** ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
** NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
** ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
** The views and conclusions contained in the software and documentation are those of the
** authors and should not be interpreted as representing official policies, either expressed
** or implied, of Benjamin Rosseaux.
*)

type PPNGPixel=^TPNGPixel;
     TPNGPixel=packed record
      r,g,b,a:{$ifdef PNGHighDepth}word{$else}byte{$endif};
     end;

type TPNGHeader=array[0..7] of byte;

{$ifndef fpc}
     ptruint=longword;
     ptrint=longint;
{$endif}

const PNGHeader:TPNGHeader=($89,$50,$4e,$47,$0d,$0a,$1a,$0a);

function CRC32(data:pointer;length:longword):longword;
const CRC32Table:array[0..15] of longword=($00000000,$1db71064,$3b6e20c8,$26d930ac,$76dc4190,
                                           $6b6b51f4,$4db26158,$5005713c,$edb88320,$f00f9344,
                                           $d6d6a3e8,$cb61b38c,$9b64c2b0,$86d3d2d4,$a00ae278,
                                           $bdbdf21c);

var buf:pansichar;
    i:longword;
begin
 if length=0 then begin
  result:=0;
 end else begin
  buf:=data;
  result:=$ffffffff;
  for i:=1 to length do begin
   result:=result xor byte(buf^);
   result:=CRC32Table[result and $f] xor (result shr 4);
   result:=CRC32Table[result and $f] xor (result shr 4);
   inc(buf);
  end;
  result:=result xor $ffffffff;
 end;
end;

function Swap16(x:word):word;
begin
 result:=((x and $ff) shl 8) or ((x and $ff00) shr 8);
end;

function Swap32(x:longword):longword;
begin
 result:=(Swap16(x and $ffff) shl 16) or Swap16((x and $ffff0000) shr 16);
end;

function Swap64(x:int64):int64;
begin
 result:=(Swap32(x and $ffffffff) shl 32) or Swap32((x and $ffffffff00000000) shr 32);
end;

function DoInflate(InData:pointer;InLen:longword;var DestData:pointer;var DestLen:longword;ParseHeader:boolean):boolean;
const CLCIndex:array[0..18] of byte=(16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15);
type pword=^word;
     PTree=^TTree;
     TTree=packed record
      Table:array[0..15] of word;
      Translation:array[0..287] of word;
     end;
     PBuffer=^TBuffer;
     TBuffer=array[0..65535] of byte;
     PLengths=^TLengths;
     TLengths=array[0..288+32-1] of byte;
     POffsets=^TOffsets;
     TOffsets=array[0..15] of word;
     PBits=^TBits;
     TBits=array[0..29] of byte;
     PBase=^TBase;
     TBase=array[0..29] of word;
var Tag,BitCount,DestSize:longword;
    SymbolLengthTree,DistanceTree,FixedSymbolLengthTree,FixedDistanceTree:PTree;
    LengthBits,DistanceBits:PBits;
    LengthBase,DistanceBase:PBase;
    Source,SourceEnd:pansichar;
    Dest:pansichar;
 procedure IncSize(length:longword);
 var j:longword;
 begin
  if (DestLen+length)>=DestSize then begin
   if DestSize=0 then begin
    DestSize:=1;
   end;
   while (DestLen+length)>=DestSize do begin
    inc(DestSize,DestSize);
   end;
   j:=ptruint(Dest)-ptruint(DestData);
   ReAllocMem(DestData,DestSize);
   ptruint(Dest):=ptruint(DestData)+j;
  end;
 end;
 function adler32(data:pointer;length:longword):longword;
 const BASE=65521;
       NMAX=5552;
 var buf:pansichar;
     s1,s2,k,i:longword;
 begin
  s1:=1;
  s2:=0;
  buf:=data;
  while length>0 do begin
   if length<NMAX then begin
    k:=length;
   end else begin
    k:=NMAX;
   end;
   dec(length,k);
   for i:=1 to k do begin
    inc(s1,byte(buf^));
    inc(s2,s1);
    inc(buf);
   end;
   s1:=s1 mod BASE;
   s2:=s2 mod BASE;
  end;
  result:=(s2 shl 16) or s1;
 end;
 procedure BuildBitsBase(Bits:pansichar;Base:pword;Delta,First:integer);
 var i,Sum:integer;
 begin
  for i:=0 to Delta-1 do begin
   Bits[i]:=ansichar(#0);
  end;
  for i:=0 to (30-Delta)-1 do begin
   Bits[i+Delta]:=ansichar(byte(i div Delta));
  end;
  Sum:=First;
  for i:=0 to 29 do begin
   Base^:=Sum;
   inc(Base);
   inc(Sum,1 shl byte(Bits[i]));
  end;
 end;
 procedure BuildFixedTrees(var lt,dt:TTree);
 var i:integer;
 begin
  for i:=0 to 6 do begin
   lt.Table[i]:=0;
  end;
  lt.Table[7]:=24;
  lt.Table[8]:=152;
  lt.Table[9]:=112;
  for i:=0 to 23 do begin
   lt.Translation[i]:=256+i;
  end;
  for i:=0 to 143 do begin
   lt.Translation[24+i]:=i;
  end;
  for i:=0 to 7 do begin
   lt.Translation[168+i]:=280+i;
  end;
  for i:=0 to 111 do begin
   lt.Translation[176+i]:=144+i;
  end;
  for i:=0 to 4 do begin
   dt.Table[i]:=0;
  end;
  dt.Table[5]:=32;
  for i:=0 to 31 do begin
   dt.Translation[i]:=i;
  end;
 end;
 procedure BuildTree(var t:TTree;Lengths:pansichar;Num:integer);
 var Offsets:POffsets;
     i:integer;
     Sum:longword;
 begin
  New(Offsets);
  try
   for i:=0 to 15 do begin
    t.Table[i]:=0;
   end;
   for i:=0 to Num-1 do begin
    inc(t.Table[byte(Lengths[i])]);
   end;
   t.Table[0]:=0;
   Sum:=0;
   for i:=0 to 15 do begin
    Offsets^[i]:=Sum;
    inc(Sum,t.Table[i]);
   end;
   for i:=0 to Num-1 do begin
    if lengths[i]<>ansichar(#0) then begin
     t.Translation[Offsets^[byte(lengths[i])]]:=i;
     inc(Offsets^[byte(lengths[i])]);
    end;
   end;
  finally
   Dispose(Offsets);
  end;
 end;
 function GetBit:longword;
 begin
  if BitCount=0 then begin
   Tag:=byte(Source^);
   inc(Source);
   BitCount:=7;
  end else begin
   dec(BitCount);
  end;
  result:=Tag and 1;
  Tag:=Tag shr 1;
 end;
 function ReadBits(Num,Base:longword):longword;
 var Limit,Mask:longword;
 begin
  result:=0;
  if Num<>0 then begin
   Limit:=1 shl Num;
   Mask:=1;
   while Mask<Limit do begin
    if GetBit<>0 then begin
     inc(result,Mask);
    end;
    Mask:=Mask shl 1;
   end;
  end;
  inc(result,Base);
 end;
 function DecodeSymbol(var t:TTree):longword;
 var Sum,c,l:integer;
 begin
  Sum:=0;
  c:=0;
  l:=0;
  repeat
   c:=(c*2)+longint(GetBit);
   inc(l);
   inc(Sum,t.Table[l]);
   dec(c,t.Table[l]);
  until not (c>=0);
  result:=t.Translation[Sum+c];
 end;
 procedure DecodeTrees(var lt,dt:TTree);
 var CodeTree:PTree;
     Lengths:PLengths;
     hlit,hdist,hclen,i,num,length,clen,Symbol,Prev:longword;
 begin
  New(CodeTree);
  New(Lengths);
  try
   FillChar(CodeTree^,sizeof(TTree),ansichar(#0));
   FillChar(Lengths^,sizeof(TLengths),ansichar(#0));
   hlit:=ReadBits(5,257);
   hdist:=ReadBits(5,1);
   hclen:=ReadBits(4,4);
   for i:=0 to 18 do begin
    lengths^[i]:=0;
   end;
   for i:=1 to hclen do begin
    clen:=ReadBits(3,0);
    lengths^[CLCIndex[i-1]]:=clen;
   end;
   BuildTree(CodeTree^,pansichar(pointer(@lengths^[0])),19);
   num:=0;
   while num<(hlit+hdist) do begin
    Symbol:=DecodeSymbol(CodeTree^);
    case Symbol of
     16:begin
      prev:=lengths^[num-1];
      length:=ReadBits(2,3);
      while length>0 do begin
       lengths^[num]:=prev;
       inc(num);
       dec(length);
      end;
     end;
     17:begin
      length:=ReadBits(3,3);
      while length>0 do begin
       lengths^[num]:=0;
       inc(num);
       dec(length);
      end;
     end;
     18:begin
      length:=ReadBits(7,11);
      while length>0 do begin
       lengths^[num]:=0;
       inc(num);
       dec(length);
      end;
     end;
     else begin
      lengths^[num]:=Symbol;
      inc(num);
     end;
    end;
   end;
   BuildTree(lt,pansichar(pointer(@lengths^[0])),hlit);
   BuildTree(dt,pansichar(pointer(@lengths^[hlit])),hdist);
  finally
   Dispose(CodeTree);
   Dispose(Lengths);
  end;
 end;
 function InflateBlockData(var lt,dt:TTree):boolean;
 var Symbol:longword;
     Length,Distance,Offset,i:integer;
 begin
  result:=false;
  while (Source<SourceEnd) or (BitCount>0) do begin
   Symbol:=DecodeSymbol(lt);
   if Symbol=256 then begin
    result:=true;
    break;
   end;
   if Symbol<256 then begin
    IncSize(1);
    Dest^:=ansichar(byte(Symbol));
    inc(Dest);
    inc(DestLen);
   end else begin
    dec(Symbol,257);
    Length:=ReadBits(LengthBits^[Symbol],LengthBase^[Symbol]);
    Distance:=DecodeSymbol(dt);
    Offset:=ReadBits(DistanceBits^[Distance],DistanceBase^[Distance]);
    IncSize(length);
    for i:=0 to length-1 do begin
     Dest[i]:=Dest[i-Offset];
    end;
    inc(Dest,Length);
    inc(DestLen,Length);
   end;
  end;
 end;
 function InflateUncompressedBlock:boolean;
 var length,invlength:longword;
 begin
  result:=false;
  length:=(byte(source[1]) shl 8) or byte(source[0]);
  invlength:=(byte(source[3]) shl 8) or byte(source[2]);
  if length<>((not invlength) and $ffff) then begin
   exit;
  end;
  IncSize(length);
  inc(Source,4);
  if Length>0 then begin
   Move(Source^,Dest^,Length);
   inc(Source,Length);
   inc(Dest,Length);
  end;
  BitCount:=0;
  inc(DestLen,Length);
  result:=true;
 end;
 function InflateFixedBlock:boolean;
 begin
  result:=InflateBlockData(FixedSymbolLengthTree^,FixedDistanceTree^);
 end;
 function InflateDynamicBlock:boolean;
 begin
  DecodeTrees(SymbolLengthTree^,DistanceTree^);
  result:=InflateBlockData(SymbolLengthTree^,DistanceTree^);
 end;
 function Uncompress:boolean;
 var Final,r:boolean;
     BlockType:longword;
 begin
  result:=false;
  BitCount:=0;
  Final:=false;
  while not Final do begin
   Final:=GetBit<>0;
   BlockType:=ReadBits(2,0);
   case BlockType of
    0:begin
     r:=InflateUncompressedBlock;
    end;
    1:begin
     r:=InflateFixedBlock;
    end;
    2:begin
     r:=InflateDynamicBlock;
    end;
    else begin
     r:=false;
    end;
   end;
   if not r then begin
    exit;
   end;
  end;
  result:=true;
 end;
 function UncompressZLIB:boolean;
 var cmf,flg:byte;
     a32:longword;
 begin
  result:=false;
  Source:=InData;
  cmf:=byte(Source[0]);
  flg:=byte(Source[1]);
  if ((((cmf shl 8)+flg) mod 31)<>0) or ((cmf and $f)<>8) or ((cmf shr 4)>7) or ((flg and $20)<>0) then begin
   exit;
  end;
  a32:=(byte(Source[InLen-4]) shl 24) or (byte(Source[InLen-3]) shl 16) or (byte(Source[InLen-2]) shl 8) or (byte(Source[InLen-1]) shl 0);
  inc(Source,2);
  dec(InLen,6);
  SourceEnd:=@Source[InLen];
  result:=Uncompress;
  if not result then begin
   exit;
  end;
  result:=adler32(DestData,DestLen)=a32;
 end;
 function UncompressDirect:boolean;
 begin
  Source:=InData;
  SourceEnd:=@Source[InLen];
  result:=Uncompress;
 end;
begin
 DestData:=nil;
 LengthBits:=nil;
 DistanceBits:=nil;
 LengthBase:=nil;
 DistanceBase:=nil;
 SymbolLengthTree:=nil;
 DistanceTree:=nil;
 FixedSymbolLengthTree:=nil;
 FixedDistanceTree:=nil;
 try
  New(LengthBits);
  New(DistanceBits);
  New(LengthBase);
  New(DistanceBase);
  New(SymbolLengthTree);
  New(DistanceTree);
  New(FixedSymbolLengthTree);
  New(FixedDistanceTree);
  try
   begin
    FillChar(LengthBits^,sizeof(TBits),ansichar(#0));
    FillChar(DistanceBits^,sizeof(TBits),ansichar(#0));
    FillChar(LengthBase^,sizeof(TBase),ansichar(#0));
    FillChar(DistanceBase^,sizeof(TBase),ansichar(#0));
    FillChar(SymbolLengthTree^,sizeof(TTree),ansichar(#0));
    FillChar(DistanceTree^,sizeof(TTree),ansichar(#0));
    FillChar(FixedSymbolLengthTree^,sizeof(TTree),ansichar(#0));
    FillChar(FixedDistanceTree^,sizeof(TTree),ansichar(#0));
   end;
   begin
    BuildFixedTrees(FixedSymbolLengthTree^,FixedDistanceTree^);
    BuildBitsBase(pansichar(pointer(@LengthBits^[0])),pword(pointer(@LengthBase^[0])),4,3);
    BuildBitsBase(pansichar(pointer(@DistanceBits^[0])),pword(pointer(@DistanceBase^[0])),2,1);
    LengthBits^[28]:=0;
    LengthBase^[28]:=258;
   end;
   begin
    GetMem(DestData,4096);
    DestSize:=4096;
    Dest:=DestData;
    DestLen:=0;
    if ParseHeader then begin
     result:=UncompressZLIB;
    end else begin
     result:=UncompressDirect;
    end;
    if result then begin
     ReAllocMem(DestData,DestLen);
    end else if assigned(DestData) then begin
     FreeMem(DestData);
     DestData:=nil;
    end;
   end;
  finally
   if assigned(LengthBits) then begin
    Dispose(LengthBits);
   end;
   if assigned(DistanceBits) then begin
    Dispose(DistanceBits);
   end;
   if assigned(LengthBase) then begin
    Dispose(LengthBase);
   end;
   if assigned(DistanceBase) then begin
    Dispose(DistanceBase);
   end;
   if assigned(SymbolLengthTree) then begin
    Dispose(SymbolLengthTree);
   end;
   if assigned(DistanceTree) then begin
    Dispose(DistanceTree);
   end;
   if assigned(FixedSymbolLengthTree) then begin
    Dispose(FixedSymbolLengthTree);
   end;
   if assigned(FixedDistanceTree) then begin
    Dispose(FixedDistanceTree);
   end;
  end;
 except
  result:=false;
 end;
end;

type PPNGPixelEx=^TPNGPixelEx;
     TPNGPixelEx=packed record
      r,g,b,a:word;
     end;

     TPNGColorFunc=function(x:int64):TPNGPixelEx;

function ColorGray1(x:int64):TPNGPixelEx;
begin
 result.r:=(0-(x and 1)) and $ffff;
 result.g:=(0-(x and 1)) and $ffff;
 result.b:=(0-(x and 1)) and $ffff;
 result.a:=$ffff;
end;

function ColorGray2(x:int64):TPNGPixelEx;
begin
 result.r:=(x and 3) or ((x and 3) shl 2) or ((x and 3) shl 4) or ((x and 3) shl 6) or ((x and 3) shl 8) or ((x and 3) shl 10) or ((x and 3) shl 12) or ((x and 3) shl 14);
 result.g:=(x and 3) or ((x and 3) shl 2) or ((x and 3) shl 4) or ((x and 3) shl 6) or ((x and 3) shl 8) or ((x and 3) shl 10) or ((x and 3) shl 12) or ((x and 3) shl 14);
 result.b:=(x and 3) or ((x and 3) shl 2) or ((x and 3) shl 4) or ((x and 3) shl 6) or ((x and 3) shl 8) or ((x and 3) shl 10) or ((x and 3) shl 12) or ((x and 3) shl 14);
 result.a:=$ffff;
end;

function ColorGray4(x:int64):TPNGPixelEx;
begin
 result.r:=(x and $f) or ((x and $f) shl 4) or ((x and $f) shl 8) or ((x and $f) shl 12);
 result.g:=(x and $f) or ((x and $f) shl 4) or ((x and $f) shl 8) or ((x and $f) shl 12);
 result.b:=(x and $f) or ((x and $f) shl 4) or ((x and $f) shl 8) or ((x and $f) shl 12);
 result.a:=$ffff;
end;

function ColorGray8(x:int64):TPNGPixelEx;
begin
 result.r:=(x and $ff) or ((x and $ff) shl 8);
 result.g:=(x and $ff) or ((x and $ff) shl 8);
 result.b:=(x and $ff) or ((x and $ff) shl 8);
 result.a:=$ffff;
end;

function ColorGray16(x:int64):TPNGPixelEx;
begin
 result.r:=x and $ffff;
 result.g:=x and $ffff;
 result.b:=x and $ffff;
 result.a:=$ffff;
end;

function ColorGrayAlpha8(x:int64):TPNGPixelEx;
begin
 result.r:=(x and $00ff) or ((x and $00ff) shl 8);
 result.g:=(x and $00ff) or ((x and $00ff) shl 8);
 result.b:=(x and $00ff) or ((x and $00ff) shl 8);
 result.a:=(x and $ff00) or ((x and $ff00) shr 8);
end;

function ColorGrayAlpha16(x:int64):TPNGPixelEx;
begin
 result.r:=(x shr 16) and $ffff;
 result.g:=(x shr 16) and $ffff;
 result.b:=(x shr 16) and $ffff;
 result.a:=x and $ffff;
end;

function ColorColor8(x:int64):TPNGPixelEx;
begin
 result.r:=(x and $ff) or ((x and $ff) shl 8);
 result.g:=((x shr 8) and $ff) or (((x shr 8) and $ff) shl 8);
 result.b:=((x shr 16) and $ff) or (((x shr 16) and $ff) shl 8);
 result.a:=$ffff;
end;

function ColorColor16(x:int64):TPNGPixelEx;
begin
 result.r:=x and $ffff;
 result.g:=(x shr 16) and $ffff;
 result.b:=(x shr 32) and $ffff;
 result.a:=$ffff;
end;

function ColorColorAlpha8(x:int64):TPNGPixelEx;
begin
 result.r:=(x and $ff) or ((x and $ff) shl 8);
 result.g:=((x shr 8) and $ff) or (((x shr 8) and $ff) shl 8);
 result.b:=((x shr 16) and $ff) or (((x shr 16) and $ff) shl 8);
 result.a:=((x shr 24) and $ff) or (((x shr 24) and $ff) shl 8);
end;

function ColorColorAlpha16(x:int64):TPNGPixelEx;
begin
 result.r:=x and $ffff;
 result.g:=(x shr 16) and $ffff;
 result.b:=(x shr 32) and $ffff;
 result.a:=(x shr 48) and $ffff;
end;

function LoadPNG(DataPointer:pointer;DataSize:longword;var ImageData:pointer;var ImageWidth,ImageHeight:integer):boolean;
type TBitsUsed=array[0..7] of longword;
     PByteArray=^TByteArray;
     TByteArray=array[0..65535] of byte;
     TColorData=int64;
const StartPoints:array[0..7,0..1] of word=((0,0),(0,0),(4,0),(0,4),(2,0),(0,2),(1,0),(0,1));
      Delta:array[0..7,0..1] of word=((1,1),(8,8),(8,8),(4,8),(4,4),(2,4),(2,2),(1,2));
      BitsUsed1Depth:TBitsUsed=($80,$40,$20,$10,$08,$04,$02,$01);
      BitsUsed2Depth:TBitsUsed=($c0,$30,$0c,$03,0,0,0,0);
      BitsUsed4Depth:TBitsUsed=($f0,$0f,0,0,0,0,0,0);
var DataEnd,DataPtr,DataNextChunk,DataPtrEx:pointer;
    ConvertColor:TPNGColorFunc;
    ByteWidth,CountBitsUsed,BitShift,UsingBitGroup,DataIndex:longword;
    DataBytes:TColorData;
    BitDepth,StartX,StartY,DeltaX,DeltaY,ImageBytesPerPixel,WidthHeight:integer;
    BitsUsed:TBitsUsed;
    SwitchLine,CurrentLine,PreviousLine:PByteArray;
    CountScanlines,ScanLineLength:array[0..7] of longword;
    ChunkLength,ChunkType,Width,Height,ColorType,Comp,Filter,Interlace,CRC,
    PalImgBytes,ImgBytes,PaletteSize,l:longword;
    First,HasTransparent,CgBI:boolean;
    Palette:array of array[0..3] of byte;
    TransparentColor:array of word;
    i,rx,ry,y,BitsPerPixel,ImageLineWidth,ImageSize,StartPass,EndPass,d:integer;
    idata,DecompressPtr:pointer;
    idatasize,idatacapacity,idataexpandedsize,LineFilter:longword;
    idataexpanded:pointer;
 function GetU8(var p:pointer):byte;
 begin
  result:=byte(p^);
  inc(pansichar(p),sizeof(byte));
 end;
 function GetU16(var p:pointer):word;
 begin
  result:=GetU8(p) shl 8;
  result:=result or GetU8(p);
 end;
 function GetU32(var p:pointer):longword;
 begin
  result:=GetU16(p) shl 16;
  result:=result or GetU16(p);
 end;
 function Paeth(a,b,c:integer):integer;
 var p,pa,pb,pc:integer;
 begin
  p:=(a+b)-c;
  pa:=abs(p-a);
  pb:=abs(p-b);
  pc:=abs(p-c);
  if (pa<=pb) and (pa<=pc) then begin
   result:=a;
  end else if pb<=pc then begin
   result:=b;
  end else begin
   result:=c;
  end;
 end;
 function CalcX(x:integer):integer;
 begin
  result:=StartX+(x*DeltaX);
 end;
 function CalcY(y:integer):integer;
 begin
  result:=StartY+(y*DeltaY);
 end;
 function GetCurrentLine(x:longword):byte;
 begin
  result:=CurrentLine^[x];
 end;
 function GetPrevSample(x:longword):byte;
 begin
  if x<ByteWidth then begin
   result:=0;
  end else begin
   result:=CurrentLine^[x-ByteWidth];
  end;
 end;
 function GetPreviousLine(x:longword):byte;
 begin
  result:=Previousline^[x];
 end;
 function GetPrevLinePrevSample(x:longword):byte;
 begin
  if x<ByteWidth then begin
   result:=0;
  end else begin
   result:=PreviousLine^[x-ByteWidth];
  end;
 end;
 function DoFilter(LineFilter:byte;Index:longword;b:byte):byte;
 var diff:byte;
 begin
  case LineFilter of
   0:begin // None
    diff:=0;
   end;
   1:begin // Sub
    diff:=GetPrevSample(index);
   end;
   2:begin // Up
    diff:=GetPreviousLine(index);
   end;
   3:begin // Average
    diff:=(GetPrevSample(index)+GetPreviousLine(index)) div 2;
   end;
   4:begin //Paeth
    diff:=Paeth(GetPrevSample(index),GetPreviousLine(index),GetPrevLinePrevSample(index));
   end;
   else begin
    diff:=0;
   end;
  end;
  result:=(b+diff) and $ff;
 end;
 function CalcColor:TColorData;
 var r:word;
     b:byte;
 begin
  if UsingBitGroup=0 then begin
   DataBytes:=0;
   if BitDepth=16 then begin
    r:=1;
    while r<ByteWidth do begin
     b:=CurrentLine^[DataIndex+r];
     CurrentLine^[DataIndex+r]:=CurrentLine^[DataIndex+longword(r-1)];
     CurrentLine^[DataIndex+longword(r-1)]:=b;
     inc(r,2);
    end;
   end;
   Move(CurrentLine^[DataIndex],DataBytes,ByteWidth);
{$ifdef big_endian}
   DataBytes:=Swap64(DataBytes);
{$endif}
   inc(DataIndex,ByteWidth);
  end;
  if ByteWidth=1 then begin
   result:=(longword(DataBytes and BitsUsed[UsingBitGroup]) and $ffffffff) shr (((CountBitsUsed-UsingBitGroup)-1)*BitShift);
   inc(UsingBitgroup);
   if UsingBitGroup>=CountBitsUsed then begin
    UsingBitGroup:=0;
   end;
  end else begin
   result:=DataBytes;
  end;
 end;
 procedure HandleScanLine(const y,CurrentPass:integer;const ScanLine:PByteArray);
 var x:integer;
     c:TColorData;
     pe:TPNGPixelEx;
     p:PPNGPixel;
 begin
  UsingBitGroup:=0;
  DataIndex:=0;
  if length(Palette)<>0 then begin
   for x:=0 to ScanlineLength[CurrentPass]-1 do begin
    c:=CalcColor;
    if c<length(Palette) then begin
     p:=PPNGPixel(pointer(@pansichar(ImageData)[((y*longint(Width))+CalcX(x))*sizeof(TPNGPixel)]));
{$ifdef PNGHighDepth}
     p^.r:=Palette[c,0] or (Palette[c,0] shl 8);
     p^.g:=Palette[c,1] or (Palette[c,1] shl 8);
     p^.b:=Palette[c,2] or (Palette[c,2] shl 8);
     p^.a:=Palette[c,3] or (Palette[c,3] shl 8);
{$else}
     p^.r:=Palette[c,0];
     p^.g:=Palette[c,1];
     p^.b:=Palette[c,2];
     p^.a:=Palette[c,3];
{$endif}
    end;
   end;
  end else begin
   if assigned(ConvertColor) then begin
    for x:=0 to ScanlineLength[CurrentPass]-1 do begin
     pe:=ConvertColor(CalcColor);
     p:=PPNGPixel(pointer(@pansichar(ImageData)[((y*longint(Width))+CalcX(x))*sizeof(TPNGPixel)]));
     if (((length(TransparentColor)=1) and ((pe.r=TransparentColor[0]) and (pe.r=TransparentColor[0]) and (pe.b=TransparentColor[0])))) or
        (((length(TransparentColor)=3) and ((pe.r=TransparentColor[0]) and (pe.r=TransparentColor[1]) and (pe.b=TransparentColor[2])))) then begin
      pe.a:=0;
     end;
{$ifdef PNGHighDepth}
     p^.r:=pe.r;
     p^.g:=pe.g;
     p^.b:=pe.b;
     p^.a:=pe.a;
{$else}
     p^.r:=pe.r shr 8;
     p^.g:=pe.g shr 8;
     p^.b:=pe.b shr 8;
     p^.a:=pe.a shr 8;
{$endif}
    end;
   end;
  end;
 end;
 procedure CgBISwapBGR2RGBandUnpremultiply;
 const UnpremultiplyFactor={$ifdef PNGHighDepth}65535{$else}255{$endif};
       FullAlpha={$ifdef PNGHighDepth}65535{$else}255{$endif};
 var i,b,a:integer;
     p:PPNGPixel;
 begin
  a:=FullAlpha;
  p:=PPNGPixel(pointer(@pansichar(ImageData)[0]));
  for i:=0 to WidthHeight-1 do begin
   a:=a and p^.a;
   inc(p);
  end;
  if ((ColorType and 4)<>0) or (a<>FullAlpha) or HasTransparent then begin
   p:=PPNGPixel(pointer(@pansichar(ImageData)[0]));
   for i:=0 to WidthHeight-1 do begin
    a:=p^.a;
    if a<>0 then begin
     b:=p^.b;
     p^.b:=(p^.r*UnpremultiplyFactor) div a;
     p^.r:=(b*UnpremultiplyFactor) div a;
     p^.g:=(p^.g*UnpremultiplyFactor) div a;
    end else begin
     b:=p^.b;
     p^.b:=p^.r;
     p^.r:=b;
    end;
    inc(p);
   end;
  end else begin
   p:=PPNGPixel(pointer(@pansichar(ImageData)[0]));
   for i:=0 to WidthHeight-1 do begin
    b:=p^.b;
    p^.b:=p^.r;
    p^.r:=b;
    inc(p);
   end;
  end;
 end;
begin
 result:=false;
 ImageData:=nil;
 try
  Palette:=nil;
  TransparentColor:=nil;
  idataexpanded:=nil;
  try
   if (assigned(DataPointer) and (DataSize>8)) and
      ((pansichar(DataPointer)[0]=#$89) and (pansichar(DataPointer)[1]=#$50) and (pansichar(DataPointer)[2]=#$4e) and (pansichar(DataPointer)[3]=#$47) and
       (pansichar(DataPointer)[4]=#$0d) and (pansichar(DataPointer)[5]=#$0a) and (pansichar(DataPointer)[6]=#$1a) and (pansichar(DataPointer)[7]=#$0a)) then begin
    DataEnd:=@pansichar(DataPointer)[DataSize];
    First:=true;
    PalImgBytes:=0;
    ImgBytes:=0;
    idata:=nil;
    DataPtr:=@pansichar(DataPointer)[8];
    Width:=0;
    Height:=0;
    idatasize:=0;
    idatacapacity:=0;
    PaletteSize:=0;
    idataexpandedsize:=0;
    BitDepth:=0;
    ColorType:=0;
    Interlace:=0;
    WidthHeight:=0;
    DataBytes:=0;
    CgBI:=false;
    HasTransparent:=false;
    while (pansichar(DataPtr)+11)<pansichar(DataEnd) do begin
     ChunkLength:=GetU32(DataPtr);
     if (pansichar(DataPtr)+(4+ChunkLength))>pansichar(DataEnd) then begin
      result:=false;
      break;
     end;
     DataPtrEx:=DataPtr;
     ChunkType:=GetU32(DataPtr);
     DataNextChunk:=@pansichar(DataPtr)[ChunkLength];
     CRC:=GetU32(DataNextChunk);
     if CRC32(DataPtrEx,ChunkLength+4)<>CRC then begin
      result:=false;
      break;
     end;
     case ChunkType of
      longword((ord('C') shl 24) or (ord('g') shl 16) or (ord('B') shl 8) or ord('I')):begin // CgBI
       CgBI:=true;
      end;
      longword((ord('I') shl 24) or (ord('H') shl 16) or (ord('D') shl 8) or ord('R')):begin // IHDR
       if ChunkLength=13 then begin
        if not First then begin
         result:=false;
         break;
        end;
        First:=false;
        Width:=GetU32(DataPtr);
        Height:=GetU32(DataPtr);
        if ((Width>(1 shl 24)) or (Height>(1 shl 24))) or ((Width=0) or (Height=0)) then begin
         result:=false;
         break;
        end;
        BitDepth:=GetU8(DataPtr);
        if not (BitDepth in [1,2,4,8]) then begin
         result:=false;
         break;
        end;
        ColorType:=GetU8(DataPtr);
        if (ColorType>6) or ((ColorType<>3) and ((ColorType and 1)<>0)) then begin
         result:=false;
         exit;
        end else if ColorType=3 then begin
         PalImgBytes:=3;
        end;
        Comp:=GetU8(DataPtr);
        if Comp<>0 then begin
         result:=false;
         break;
        end;
        Filter:=GetU8(DataPtr);
        if Filter<>0 then begin
         result:=false;
         break;
        end;
        Interlace:=GetU8(DataPtr);
        if Interlace>1 then begin
         result:=false;
         break;
        end;
        if PalImgBytes=0 then begin
         if (ColorType and 2)<>0 then begin
          ImgBytes:=3;
         end else begin
          ImgBytes:=1;
         end;
         if (ColorType and 4)<>0 then begin
          inc(ImgBytes);
         end;
         if (((1 shl 30) div Width) div ImgBytes)<Height then begin
          result:=false;
          break;
         end;
        end else begin
         ImgBytes:=1;
         if (((1 shl 30) div Width) div 4)<Height then begin
          result:=false;
          break;
         end;
        end;
       end else begin
        result:=false;
        break;
       end;
      end;
      longword((ord('P') shl 24) or (ord('L') shl 16) or (ord('T') shl 8) or ord('E')):begin // PLTE
       if First then begin
        result:=false;
        break;
       end;
       case PalImgBytes of
        3:begin
         PaletteSize:=ChunkLength div 3;
         if (PaletteSize*3)<>ChunkLength then begin
          result:=false;
          break;
         end;
         SetLength(Palette,PaletteSize);
         for i:=0 to PaletteSize-1 do begin
          Palette[i,0]:=GetU8(DataPtr);
          Palette[i,1]:=GetU8(DataPtr);
          Palette[i,2]:=GetU8(DataPtr);
          Palette[i,3]:=$ff;
         end;
        end;
        4:begin
         PaletteSize:=ChunkLength div 4;
         if (PaletteSize*4)<>ChunkLength then begin
          result:=false;
          exit;
         end;
         SetLength(Palette,PaletteSize);
         for i:=0 to PaletteSize-1 do begin
          Palette[i,0]:=GetU8(DataPtr);
          Palette[i,1]:=GetU8(DataPtr);
          Palette[i,2]:=GetU8(DataPtr);
          Palette[i,3]:=GetU8(DataPtr);
         end;
        end;
        else begin
         result:=false;
         break;
        end;
       end;
      end;
      longword((ord('t') shl 24) or (ord('R') shl 16) or (ord('N') shl 8) or ord('S')):begin // tRNS
       if First or assigned(idata) then begin
        result:=false;
        break;
       end;
       if PalImgBytes<>0 then begin
        if (length(Palette)=0) or (longint(ChunkLength)>length(Palette)) then begin
         result:=false;
         break;
        end;
        PalImgBytes:=4;
        for i:=0 to PaletteSize-1 do begin
         Palette[i,3]:=GetU8(DataPtr);
        end;
       end else begin
        if ChunkLength=ImgBytes then begin
         SetLength(TransparentColor,longint(ImgBytes));
         for i:=0 to longint(ImgBytes)-1 do begin
          d:=GetU8(DataPtr);
          TransparentColor[i]:=d or (d shl 8);
         end;
        end else begin
         if ((ImgBytes and 1)=0) or (ChunkLength<>(ImgBytes*2)) then begin
          result:=false;
          break;
         end;
         HasTransparent:=true;
         SetLength(TransparentColor,longint(ImgBytes));
         for i:=0 to longint(ImgBytes)-1 do begin
          TransparentColor[i]:=GetU16(DataPtr);
         end;
        end;
       end;
      end;
      longword((ord('I') shl 24) or (ord('D') shl 16) or (ord('A') shl 8) or ord('T')):begin // IDAT
       if First or ((PalImgBytes<>0) and (length(Palette)=0)) then begin
        result:=false;
        break;
       end;
       if (idatasize=0) or (idatacapacity=0) or not assigned(idata) then begin
        idatasize:=ChunkLength;
        idatacapacity:=ChunkLength;
        GetMem(idata,idatacapacity);
        Move(DataPtr^,idata^,ChunkLength);
       end else begin
        if (idatasize+ChunkLength)>=idatacapacity then begin
         if idatacapacity=0 then begin
          idatacapacity:=1;
         end;
         while (idatasize+ChunkLength)>=idatacapacity do begin
          inc(idatacapacity,idatacapacity);
         end;
         ReallocMem(idata,idatacapacity);
        end;
        Move(DataPtr^,pansichar(idata)[idatasize],ChunkLength);
        inc(idatasize,ChunkLength);
       end;
      end;
      longword((ord('I') shl 24) or (ord('E') shl 16) or (ord('N') shl 8) or ord('D')):begin // IEND
       if First or ((PalImgBytes<>0) and (length(Palette)=0)) or not assigned(idata) then begin
        result:=false;
        break;
       end;
       if not DoInflate(idata,idatasize,idataexpanded,idataexpandedsize,not CgBI) then begin
        result:=false;
        break;
       end;
       BitsPerPixel:=longint(ImgBytes)*BitDepth;
       ImageWidth:=Width;
       ImageHeight:=Height;
       WidthHeight:=Width*Height;
       ImageBytesPerPixel:=((longint(ImgBytes)*longint(BitDepth))+7) shr 3;
       ImageLineWidth:=((ImageWidth*BitsPerPixel)+7) shr 3;
       ImageSize:=(((ImageWidth*ImageHeight)*BitsPerPixel)+7) shr 3;
       GetMem(ImageData,(ImageWidth*ImageHeight)*sizeof(TPNGPixel));
       try
        CountBitsUsed:=0;
        case Interlace of
         0:begin
          StartPass:=0;
          EndPass:=0;
          CountScanlines[0]:=Height;
          ScanLineLength[0]:=Width;
         end;
         1:begin
          StartPass:=1;
          EndPass:=7;
          for i:=1 to 7 do begin
           d:=Height div Delta[i,1];
           if (Height mod Delta[i,1])>StartPoints[i,1] then begin
            inc(d);
           end;
           CountScanLines[i]:=d;
           d:=Width div Delta[i,0];
           if (Width mod Delta[i,0])>StartPoints[i,0] then begin
            inc(d);
           end;
           ScanLineLength[i]:=d;
          end;
         end;
         else begin
          if assigned(ImageData) then begin
           FreeMem(ImageData);
           ImageData:=nil;
          end;
          result:=false;
          break;
         end;
        end;
        ByteWidth:=0;
        ConvertColor:=nil;
        case ColorType of
         0:begin
          case Bitdepth of
           1:begin
            ConvertColor:=@ColorGray1;
            ByteWidth:=1;
           end;
           2:begin
            ConvertColor:=@ColorGray2;
            ByteWidth:=1;
           end;
           4:begin
            ConvertColor:=@ColorGray4;
            ByteWidth:=1;
           end;
           8:begin
            ConvertColor:=@ColorGray8;
            ByteWidth:=1;
           end;
           16:begin
            ConvertColor:=@ColorGray16;
            ByteWidth:=2;
           end;
          end;
         end;
         2:begin
          if BitDepth=8 then begin
           ConvertColor:=@ColorColor8;
           ByteWidth:=3;
          end else begin
           ConvertColor:=@ColorColor16;
           ByteWidth:=6;
          end;
         end;
         3:begin
          if BitDepth=16 then begin
           ByteWidth:=2;
          end else begin
           ByteWidth:=1;
          end;
         end;
         4:begin
          if BitDepth=8 then begin
           ConvertColor:=@ColorGrayAlpha8;
           ByteWidth:=2;
          end else begin
           ConvertColor:=@ColorGrayAlpha16;
           ByteWidth:=4;
          end;
         end;
         6:begin
          if BitDepth=8 then begin
           ConvertColor:=@ColorColorAlpha8;
           ByteWidth:=4;
          end else begin
           ConvertColor:=@ColorColorAlpha16;
           ByteWidth:=8;
          end;
         end;
        end;
        case BitDepth of
         1:begin
          CountBitsUsed:=8;
          BitShift:=1;
          BitsUsed:=BitsUsed1Depth;
         end;
         2:begin
          CountBitsUsed:=4;
          BitShift:=2;
          BitsUsed:=BitsUsed2Depth;
         end;
         4:begin
          CountBitsUsed:=2;
          BitShift:=4;
          BitsUsed:=BitsUsed4Depth;
         end;
         8:begin
          CountBitsUsed:=1;
          BitShift:=0;
          BitsUsed[0]:=$ff;
         end;
        end;
        DecompressPtr:=idataexpanded;
        for i:=StartPass to EndPass do begin
         StartX:=StartPoints[i,0];
         StartY:=StartPoints[i,1];
         DeltaX:=Delta[i,0];
         DeltaY:=Delta[i,1];
         if ByteWidth=1 then begin
          l:=ScanLineLength[i] div CountBitsUsed;
          if (ScanLineLength[i] mod CountBitsUsed)>0 then begin
           inc(l);
          end;
         end else begin
          l:=ScanLineLength[i]*ByteWidth;
         end;
         GetMem(PreviousLine,l);
         GetMem(CurrentLine,l);
         try
          FillChar(CurrentLine^,l,ansichar(#0));
          for ry:=0 to CountScanlines[i]-1 do begin
           SwitchLine:=CurrentLine;
           CurrentLine:=PreviousLine;
           PreviousLine:=SwitchLine;
           y:=CalcY(ry);
           LineFilter:=GetU8(DecompressPtr);
           Move(DecompressPtr^,CurrentLine^,l);
           inc(pansichar(DecompressPtr),l);
           if LineFilter<>0 then begin
            for rx:=0 to l-1 do begin
             CurrentLine^[rx]:=DoFilter(LineFilter,rx,CurrentLine^[rx]);
            end;
           end;
           HandleScanLine(y,i,CurrentLine);
          end;
         finally
          FreeMem(PreviousLine);
          FreeMem(CurrentLine);
         end;
        end;
        if CgBI then begin
         CgBISwapBGR2RGBandUnpremultiply;
        end;
       finally
       end;
       result:=true;
       break;
      end;
      else begin
      end;
     end;
     DataPtr:=DataNextChunk;
    end;
   end;
  finally
   SetLength(Palette,0);
   SetLength(TransparentColor,0);
   if assigned(idata) then begin
    FreeMem(idata);
    idata:=nil;
   end;
   if assigned(idataexpanded) then begin
    FreeMem(idataexpanded);
    idataexpanded:=nil;
   end;
  end;
 except
  if assigned(ImageData) then begin
   FreeMem(ImageData);
   ImageData:=nil;
  end;
  result:=false;
 end;
end;

{Copyright (c) 2012 Ville Krumlinde

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.}

type
  //Original code by Martin Fiedler http://keyj.s2000.ws/?p=137
  //Delphi port by Ville Krumlinde
  TNjDecoder = class
  strict private
    type ErrorCodes = (
            OK = 0,        // decoding successful
            NotAJpeg,      // not a JPEG file
            Unsupported,   // unsupported format
            OutOfMemory,   // out of memory
            InternalError, // internal error
            SyntaxError,   // syntax error
            Internal_Finished); // used internally, will never be reported
    type VlcCode = packed record
      bits,code : byte;
    end;
    type PVlcCode = ^VlcCode;
    type Component = record
      cid,
      ssx, ssy,
      width, height,
      stride,
      qtsel,
      actabsel, dctabsel,
      dcpred : integer;
      pixels : PByte;
    end;
    type PComponent = ^Component;
    type Context = record
      error : ErrorCodes;
      pos : PByte;
      size,
      length,
      width, height,
      mbwidth, mbheight,
      mbsizex, mbsizey,
      ncomp : integer;
      comp : array[0..2] of Component;
      qtused, qtavail : integer;
      qtab : array[0..3,0..63] of byte;
      vlctab : array[0..3,0..65535] of VlcCode;
      buf, bufbits : integer;
      block : array[0..63] of integer;
      rstinterval : integer;
      rgb : PByte;
    end;
    var
      ctx : Context;
    const
      ZZ : array[0..63] of byte = (0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18,
        11, 4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28, 35,
        42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45,
        38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63);
      W1 = 2841;
      W2 = 2676;
      W3 = 2408;
      W5 = 1609;
      W6 = 1108;
      W7 = 565;
    procedure RowIDCT(blk : PInteger);
    procedure ColIDCT(blk : PInteger; pout : PByte; stride : integer);
    function ShowBits(bits : integer) : integer;
    procedure ByteAlign;
    function Decode16(pos: PByte): word;
    procedure DecodeLength;
    function GetBits(bits: integer): integer;
    procedure njThrow(const E: ErrorCodes);
    procedure Skip(count: integer);
    procedure SkipBits(bits: integer);
    procedure SkipMarker;
    procedure DecodeSOF;
    procedure DecodeDHT;
    procedure DecodeBlock(c: PComponent; pout: PByte);
    procedure DecodeDQT;
    procedure DecodeDRI;
    function GetVLC(vlc: PVlcCode; code: PByte): integer;
    procedure DecodeScan;
    {$ifdef USE_CHROMA_FILTER}
    procedure UpsampleV(c: PComponent);
    procedure UpsampleH(c: PComponent);
    {$else}
    procedure Upsample(c: PComponent);
    {$endif}
    procedure Convert;
  private
    class function Clip(const X : integer) : byte;
  public
    function Decode(jpeg: pointer; const size: integer): boolean;
    destructor Destroy; override;
    function GetWidth : integer;
    function GetHeight : integer;
    function GetImage : pointer;
  end;

{$POINTERMATH ON}


//Delphi shr does not work with negative values, use this instead
function cshr(const i : integer; const j : byte) : integer;
{$if defined(CPU386)}
asm
  mov cl,j
  sar eax,cl
end;
{$elseif defined(CPUX64)}
asm
  mov eax,i
  mov cl,j
  sar eax,cl
end;
{$elseif defined(Android)}
asm
  asr r0,r0,r1
end;

{$ifend}

{ TNjDecoder }

class function TNjDecoder.Clip(const X: integer): byte;
begin
  if X<0 then
    Result := 0
  else if X>$FF then
    Result := $FF
  else
    Result := X;
end;

procedure TNjDecoder.RowIDCT(blk: PInteger);
var
  x0, x1, x2, x3, x4, x5, x6, x7, x8 : integer;
begin
  x1 := blk[4] shl 11;
  x2 := blk[6];
  x3 := blk[2];
  x4 := blk[1];
  x5 := blk[7];
  x6 := blk[5];
  x7 := blk[3];

  if (x1 or x2 or x3 or x4 or x5 or x6 or x7) = 0 then
  begin
    blk[0] := blk[0] shl 3;
    blk[1] := blk[0];
    blk[2] := blk[0];
    blk[3] := blk[0];
    blk[4] := blk[0];
    blk[5] := blk[0];
    blk[6] := blk[0];
    blk[7] := blk[0];
    Exit;
  end;

  x0 := (blk[0] shl 11) + 128;
  x8 := W7 * (x4 + x5);
  x4 := x8 + (W1 - W7) * x4;
  x5 := x8 - (W1 + W7) * x5;
  x8 := W3 * (x6 + x7);
  x6 := x8 - (W3 - W5) * x6;
  x7 := x8 - (W3 + W5) * x7;
  x8 := x0 + x1;
  x0 := x0 - x1;
  x1 := W6 * (x3 + x2);
  x2 := x1 - (W2 + W6) * x2;
  x3 := x1 + (W2 - W6) * x3;
  x1 := x4 + x6;
  x4 := x4 - x6;
  x6 := x5 + x7;
  x5 := x5 - x7;
  x7 := x8 + x3;
  x8 := x8 - x3;
  x3 := x0 + x2;
  x0 := x0 - x2;
  x2 := cshr((181 * (x4 + x5) + 128), 8);
  x4 := cshr((181 * (x4 - x5) + 128), 8);
  blk[0] := cshr((x7 + x1), 8);
  blk[1] := cshr((x3 + x2), 8);
  blk[2] := cshr((x0 + x4), 8);
  blk[3] := cshr((x8 + x6), 8);
  blk[4] := cshr((x8 - x6), 8);
  blk[5] := cshr((x0 - x4), 8);
  blk[6] := cshr((x3 - x2), 8);
  blk[7] := cshr((x7 - x1), 8);
end;

procedure TNjDecoder.ColIDCT(blk : PInteger; pout : PByte; stride : integer);
var
  x0, x1, x2, x3, x4, x5, x6, x7, x8 : integer;
begin
  x1 := blk[8*4] shl 8;
  x2 := blk[8*6];
  x3 := blk[8*2];
  x4 := blk[8*1];
  x5 := blk[8*7];
  x6 := blk[8*5];
  x7 := blk[8*3];

  if (x1 or x2 or x3 or x4 or x5 or x6 or x7) = 0 then
  begin
    x1 := Clip( cshr((blk[0] + 32), 6) + 128);
    for x0 := 8 downto 1 do
    begin
      pout^ := x1;
      Inc(pout,stride);
    end;
    Exit;
  end;

  x0 := (blk[0] shl 8) + 8192;
  x8 := W7 * (x4 + x5) + 4;
  x4 := cshr((x8 + (W1 - W7) * x4), 3);
  x5 := cshr((x8 - (W1 + W7) * x5), 3);
  x8 := W3 * (x6 + x7) + 4;
  x6 := cshr((x8 - (W3 - W5) * x6), 3);
  x7 := cshr((x8 - (W3 + W5) * x7), 3);
  x8 := x0 + x1;
  x0 := x0 - x1;
  x1 := W6 * (x3 + x2) + 4;
  x2 := cshr((x1 - (W2 + W6) * x2), 3);
  x3 := cshr((x1 + (W2 - W6) * x3), 3);
  x1 := x4 + x6;
  x4 := x4 - x6;
  x6 := x5 + x7;
  x5 := x5 - x7;
  x7 := x8 + x3;
  x8 := x8 - x3;
  x3 := x0 + x2;
  x0 := x0 - x2;
  x2 := cshr((181 * (x4 + x5) + 128), 8);
  x4 := cshr((181 * (x4 - x5) + 128), 8);
  pout^ := Clip((cshr((x7 + x1), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x3 + x2), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x0 + x4), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x8 + x6), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x8 - x6), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x0 - x4), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x3 - x2), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x7 - x1), 14)) + 128);
end;

function TNjDecoder.ShowBits(bits : integer) : integer;
var
  newbyte,marker : byte;
begin
  if Bits=0 then
    Exit(0);

  while (ctx.bufbits < bits) do
  begin
    if (ctx.size <= 0) then
    begin
      ctx.buf := (ctx.buf shl 8) or $FF;
      ctx.bufbits := ctx.bufbits + 8;
      continue;
    end;
    newbyte := ctx.pos^; Inc(ctx.pos);
    Dec(ctx.size);
    ctx.bufbits := ctx.bufbits + 8;
    ctx.buf := (ctx.buf shl 8) or newbyte;
    if (newbyte = $FF) then
    begin
      if (ctx.size<>0) then
      begin
        marker := ctx.pos^; Inc(ctx.pos);
        Dec(ctx.size);
        case marker of
          0 : ;
          $D9 : ctx.Size:=0;
        else
          begin
            if (marker and $f8)<>$d0 then
              ctx.error := SyntaxError
            else
            begin
              ctx.buf := (ctx.buf shl 8) or marker;
              ctx.bufbits := ctx.bufbits + 8;
            end;
          end;
        end;
      end else
        ctx.error := SyntaxError;
    end;
  end;

  Result := (ctx.buf shr (ctx.bufbits - bits)) and ((1 shl bits) - 1);
end;

procedure TNjDecoder.SkipBits(bits : integer);
begin
  if (ctx.bufbits < bits) then
    ShowBits(bits);
  ctx.bufbits := ctx.bufbits-bits;
end;

function TNjDecoder.GetBits(bits : integer) : integer;
begin
  Result := ShowBits(bits);
  SkipBits(bits);
end;

function TNjDecoder.GetHeight: integer;
begin
  Result := Ctx.height;
end;

function TNjDecoder.GetImage: pointer;
begin
  if ctx.ncomp = 1 then
    Result := ctx.comp[0].pixels
  else
    Result := ctx.rgb;
end;

procedure TNjDecoder.ByteAlign;
begin
  ctx.bufbits := ctx.bufbits and $F8;
end;

procedure TNjDecoder.Skip(count : integer);
begin
  ctx.pos := ctx.pos + count;
  ctx.size := ctx.size - count;
  ctx.length := ctx.length - count;
  if (ctx.size < 0) then
    ctx.error := SyntaxError;
end;

function TNjDecoder.Decode16(pos : PByte) : word;
begin
  Result := (pos[0] shl 8) or pos[1];
end;

procedure TNjDecoder.njThrow(const E : ErrorCodes);
begin
  ctx.Error := E;
end;

procedure TNjDecoder.DecodeLength;
begin
  if (ctx.size < 2) then
    njThrow(SyntaxError);
  ctx.length := Decode16(ctx.pos);
  if (ctx.length > ctx.size) then
    njThrow(SyntaxError);
  Skip(2);
end;

procedure TNjDecoder.SkipMarker;
begin
  DecodeLength();
  Skip(ctx.length);
end;

procedure TNjDecoder.DecodeSOF;
var
  i, ssxmax, ssymax : integer;
  c : ^Component;
begin
  ssxmax := 0; ssymax :=0;
  DecodeLength();
  if (ctx.length < 9) then
    njThrow(SyntaxError);
  if (ctx.pos[0] <> 8) then
    njThrow(Unsupported);
  ctx.height := Decode16(ctx.pos+1);
  ctx.width := Decode16(ctx.pos+3);
  ctx.ncomp := ctx.pos[5];
  Skip(6);
  case (ctx.ncomp) of
    1,3 : ;
  else
    njThrow(Unsupported);
  end;

  if (ctx.length < (ctx.ncomp * 3)) then
    njThrow(SyntaxError);

  c := @ctx.comp;
  for i := 0 to ctx.ncomp-1 do
  begin
    c.cid := ctx.pos[0];

    c.ssx := ctx.pos[1] shr 4;
    if c.ssx=0 then
      njThrow(SyntaxError);

    if (c.ssx and (c.ssx - 1))<>0 then
      njThrow(Unsupported);  // non-power of two

    c.ssy := ctx.pos[1] and 15;
    if c.ssy=0 then
      njThrow(SyntaxError);

    if (c.ssy and (c.ssy - 1))<>0 then
      njThrow(Unsupported);  // non-power of two

    c.qtsel := ctx.pos[2];
    if ((c.qtsel) and $FC)<>0 then
      njThrow(SyntaxError);

    Skip(3);
    ctx.qtused := ctx.qtused or (1 shl c.qtsel);
    if (c.ssx > ssxmax) then ssxmax := c.ssx;
    if (c.ssy > ssymax) then ssymax := c.ssy;
    Inc(C);
  end;
  ctx.mbsizex := ssxmax shl 3;
  ctx.mbsizey := ssymax shl 3;
  ctx.mbwidth := (ctx.width + ctx.mbsizex - 1) div ctx.mbsizex;
  ctx.mbheight := (ctx.height + ctx.mbsizey - 1) div ctx.mbsizey;

  c := @ctx.comp;
  for i := 0 to ctx.ncomp-1 do
  begin
    c.width := (ctx.width * c.ssx + ssxmax - 1) div ssxmax;
    c.stride := (c.width + 7) and $7FFFFFF8;
    c.height := (ctx.height * c.ssy + ssymax - 1) div ssymax;
    c.stride := ctx.mbwidth * ctx.mbsizex * c.ssx div ssxmax;

    if (((c.width < 3) and (c.ssx <> ssxmax)) or ((c.height < 3) and (c.ssy <> ssymax))) then
      njThrow(Unsupported);

    GetMem(c.pixels,c.stride * (ctx.mbheight * ctx.mbsizey * c.ssy div ssymax));
    if c.pixels=nil then
      njThrow(OutOfMemory);

    Inc(C);
  end;
  if (ctx.ncomp = 3) then
  begin
    GetMem(ctx.rgb, ctx.width * ctx.height * ctx.ncomp);
    if ctx.rgb=nil then
      njThrow(OutOfMemory);
  end;
  Skip(ctx.length);
end;

destructor TNjDecoder.Destroy;
var
  i : integer;
begin
  for i := 0 to 3-1 do
    if (ctx.comp[i].pixels<>nil) then
      FreeMem(ctx.comp[i].pixels);
  if (ctx.rgb<>nil) then
    FreeMem(ctx.rgb);
  inherited;
end;

procedure TNjDecoder.DecodeDHT;
var
  codelen, currcnt, remain, spread, i, j : integer;
  vlc : PVlcCode;
  counts : array[0..15] of byte;
  code : byte;
begin
  DecodeLength();
  while (ctx.length >= 17) do
  begin
    i := ctx.pos[0];
    if (i and $EC)<>0 then njThrow(SyntaxError);
    if (i and $02)<>0 then njThrow(Unsupported);
    i := (i or (i shr 3)) and 3;  // combined DC/AC + tableid value

    for codelen := 1 to 16 do
      counts[codelen - 1] := ctx.pos[codelen];
    Skip(17);
    vlc := @ctx.vlctab[i][0];
    spread := 65536;
    remain := spread;
    for codelen := 1 to 16 do
    begin
      spread := spread div (1 shl 1);
      currcnt := counts[codelen - 1];
      if (currcnt=0) then
        Continue;
      if (ctx.length < currcnt) then
        njThrow(SyntaxError);
      remain := remain - (currcnt shl (16 - codelen));
      if (remain < 0) then
        njThrow(SyntaxError);
      for i := 0 to currcnt-1 do
      begin
        code := ctx.pos[i];
        for j := spread downto 1 do
        begin
          vlc.bits := codelen;
          vlc.code := code;
          Inc(vlc);
        end;
      end;
      Skip(currcnt);
    end;
    while (remain>0) do
    begin
      vlc.bits := 0;
      Inc(vlc);
      Dec(Remain);
    end;
  end;
  if (ctx.length<>0) then
    njThrow(SyntaxError);
end;

procedure TNjDecoder.DecodeDQT;
var
  i : integer;
  t : PByte;
begin
  DecodeLength();
  while (ctx.length >= 65) do
  begin
    i := ctx.pos[0];
    if (i and $FC)<>0 then
      njThrow(SyntaxError);
    ctx.qtavail := ctx.qtavail or (1 shl i);
    t := @ctx.qtab[i][0];
    for i := 0 to 64-1 do
      t[i] := ctx.pos[i + 1];
    Skip(65);
  end;
  if (ctx.length<>0) then
    njThrow(SyntaxError);
end;

procedure TNjDecoder.DecodeDRI;
begin
  DecodeLength();
  if (ctx.length < 2) then
    njThrow(SyntaxError);
  ctx.rstinterval := Decode16(ctx.pos);
  Skip(ctx.length);
end;

function TNjDecoder.GetVLC(vlc : PVlcCode; code : PByte) : integer;
var
  value,bits : integer;
begin
  value := ShowBits(16);
  bits := vlc[value].bits;
  if bits=0 then
  begin
    ctx.error := SyntaxError;
    Exit(0);
  end;
  SkipBits(bits);
  value := vlc[value].code;
  if (code<>nil) then
    code^ := value;
  bits := value and 15;
  if (bits=0) then
    Exit(0);
  value := GetBits(bits);
  if (value < (1 shl (bits - 1))) then
    value := value + (((-1) shl bits) + 1);
  Result := value;
end;

function TNjDecoder.GetWidth: integer;
begin
  Result := Ctx.width;
end;

procedure TNjDecoder.DecodeBlock(c : PComponent; pout : PByte);
var
  Code :  byte;
  Value, Coef : integer;
begin
  code := 0;
  coef := 0;
  FillChar(ctx.block, SizeOf(Ctx.Block), 0);
  c.dcpred := c.dcpred + GetVLC(@ctx.vlctab[c.dctabsel][0], nil);
  ctx.block[0] := (c.dcpred) * ctx.qtab[c.qtsel][0];
  repeat
    value := GetVLC(@ctx.vlctab[c.actabsel][0], @code);
    if (code=0) then
      break;  // EOB
    if ( ((code and $0F)=0) and (code <> $F0)) then
      njThrow(SyntaxError);
    coef := coef + (code shr 4) + 1;
    if (coef > 63) then
      njThrow(SyntaxError);
    ctx.block[ ZZ[coef] ] := value * ctx.qtab[c.qtsel][coef];
  until Coef>=63;
  coef := 0;
  while Coef<64 do
  begin
    RowIDCT(@ctx.block[coef]);
    Inc(Coef,8);
  end;
  for coef := 0 to 8-1 do
  begin
    ColIDCT(@ctx.block[coef], @pout[coef], c.stride);
  end;
end;

procedure TNjDecoder.DecodeScan;
var
  i, mbx, mby, sbx, sby, rstcount, nextrst : integer;
  c : PComponent;
begin
  rstcount := ctx.rstinterval;
  nextrst := 0;
  DecodeLength();
  if (ctx.length < (4 + 2 * ctx.ncomp)) then
    njThrow(SyntaxError);

  if (ctx.pos[0] <> ctx.ncomp) then
    njThrow(Unsupported);

  Skip(1);
  c := @ctx.comp;
  for i := 0 to ctx.ncomp-1 do
  begin
    if (ctx.pos[0] <> c.cid) then
      njThrow(SyntaxError);
    if (ctx.pos[1] and $EE)<>0 then
      njThrow(SyntaxError);
    c.dctabsel := (ctx.pos[1] shr 4);
    c.actabsel := (ctx.pos[1] and 1) or 2;
    Skip(2);
    Inc(C);
  end;

  if ((ctx.pos[0]<>0) or (ctx.pos[1] <> 63) or (ctx.pos[2]<>0)) then
    njThrow(Unsupported);

  Skip(ctx.length);
  mbx := 0; mby := 0;
  while True do
  begin
    c := @ctx.comp;
    for i := 0 to ctx.ncomp-1 do
    begin
      for sby := 0 to c.ssy-1 do
        for sbx := 0 to c.ssx-1 do
        begin
          DecodeBlock(c, @c.pixels[((mby * c.ssy + sby) * c.stride + mbx * c.ssx + sbx) shl 3]);
          if ctx.error>OK then
            Exit;
        end;
      Inc(C);
    end;
    Inc(mbx);
    if (mbx >= ctx.mbwidth) then
    begin
      mbx := 0;
      Inc(mby);
      if mby >= ctx.mbheight then
        Break;
    end;
    Dec(rstcount);
    if (ctx.rstinterval<>0) and (rstcount=0) then
    begin
      ByteAlign();
      i := GetBits(16);
      if (((i and $FFF8) <> $FFD0) or ((i and 7) <> nextrst)) then
        njThrow(SyntaxError);
      nextrst := (nextrst + 1) and 7;
      rstcount := ctx.rstinterval;
      for i := 0 to 3-1 do
        ctx.comp[i].dcpred := 0;
    end;
  end;
  ctx.error := Internal_Finished;
end;

{$ifdef USE_CHROMA_FILTER}

const
  CF4A = (-9);
  CF4B = (111);
  CF4C = (29);
  CF4D = (-3);
  CF3A = (28);
  CF3B = (109);
  CF3C = (-9);
  CF3X = (104);
  CF3Y = (27);
  CF3Z = (-3);
  CF2A = (139);
  CF2B = (-11);

function CF(const X : integer) : integer;
begin
  Result := TNjDecoder.clip( cshr((x + 64), 7) );
end;

procedure TNjDecoder.UpsampleH(c : PComponent);
var
  xmax,x,y : integer;
  pout,lin,lout : PByte;
begin
  xmax := c.width - 3;
  GetMem(pout, (c.width * c.height) shl 1);
  if (pout=nil) then
    njThrow(OutOfMemory);
  lin := c.pixels;
  lout := pout;
  for y := c.height downto 1 do
  begin
    lout[0] := CF(CF2A * lin[0] + CF2B * lin[1]);
    lout[1] := CF(CF3X * lin[0] + CF3Y * lin[1] + CF3Z * lin[2]);
    lout[2] := CF(CF3A * lin[0] + CF3B * lin[1] + CF3C * lin[2]);
    for x := 0 to xmax-1 do
    begin
      lout[(x shl 1) + 3] := CF(CF4A * lin[x] + CF4B * lin[x + 1] + CF4C * lin[x + 2] + CF4D * lin[x + 3]);
      lout[(x shl 1) + 4] := CF(CF4D * lin[x] + CF4C * lin[x + 1] + CF4B * lin[x + 2] + CF4A * lin[x + 3]);
    end;
    lin := lin + c.stride;
    lout := lout + (c.width shl 1);
    lout[-3] := CF(CF3A * lin[-1] + CF3B * lin[-2] + CF3C * lin[-3]);
    lout[-2] := CF(CF3X * lin[-1] + CF3Y * lin[-2] + CF3Z * lin[-3]);
    lout[-1] := CF(CF2A * lin[-1] + CF2B * lin[-2]);
  end;
  c.width := c.width shl 1;
  c.stride := c.width;
  FreeMem(c.pixels);
  c.pixels := pout;
end;

procedure TNjDecoder.UpsampleV(c : PComponent);
var
  w,s1,s2,x,y : integer;
  pout,cin,cout : PByte;
begin
  w := c.width;
  s1 := c.stride;
  s2 := s1 + s1;
  GetMem(pout,(c.width * c.height) shl 1);
  if (pout=nil) then
    njThrow(OutOfMemory);
  for x := 0 to w-1 do
  begin
    cin := @c.pixels[x];
    cout := @pout[x];
    cout^ := CF(CF2A * cin[0] + CF2B * cin[s1]); cout := cout + w;
    cout^ := CF(CF3X * cin[0] + CF3Y * cin[s1] + CF3Z * cin[s2]); cout := cout + w;
    cout^ := CF(CF3A * cin[0] + CF3B * cin[s1] + CF3C * cin[s2]); cout := cout + w;
    cin := cin + s1;
    for y := c.height - 3 downto 1 do
    begin
      cout^ := CF(CF4A * cin[-s1] + CF4B * cin[0] + CF4C * cin[s1] + CF4D * cin[s2]); cout := cout + w;
      cout^ := CF(CF4D * cin[-s1] + CF4C * cin[0] + CF4B * cin[s1] + CF4A * cin[s2]); cout := cout + w;
      cin := cin + s1;
    end;
    cin := cin + s1;
    cout^ := CF(CF3A * cin[0] + CF3B * cin[-s1] + CF3C * cin[-s2]); cout := cout + w;
    cout^ := CF(CF3X * cin[0] + CF3Y * cin[-s1] + CF3Z * cin[-s2]); cout := cout + w;
    cout^ := CF(CF2A * cin[0] + CF2B * cin[-s1]);
  end;
  c.height := c.height shl 1;
  c.stride := c.width;
  FreeMem(c.pixels);
  c.pixels := pout;
end;

{$else}

procedure TNjDecoder.Upsample(c : PComponent);
var
  x, y, xshift, yshift : integer;
  pout, lin, lout : PByte;
begin
  xshift := 0; yshift := 0;

  while (c.width < ctx.width) do
  begin
    c.width := c.width shl 1;
    Inc(xshift);
  end;

  while (c.height < ctx.height) do
  begin
    c.height := c.height shl 1;
    Inc(yshift);
  end;

  GetMem(pout, c.width * c.height);
  if pout=nil then
    njThrow(OutOfMemory);
  lout := pout;
  for y := 0 to c.height-1 do
  begin
    lin := @c.pixels[(y shr yshift) * c.stride];
    for x := 0 to c.width-1 do
      lout[x] := lin[(x shr xshift)];
    lout := lout + c.width;
  end;
  c.stride := c.width;
  FreeMem(c.pixels);
  c.pixels := pout;
end;

{$endif}

procedure TNjDecoder.Convert;
var
  i : integer;
  c : PComponent;
  x, yy : integer;
  prgb,py,pcb,pcr,pin,pout : PByte;
  y,cb,cr : integer;
begin
  c := @ctx.comp;
  for i := 0 to ctx.ncomp-1 do
  begin
    {$ifdef USE_CHROMA_FILTER}
    while (c.width < ctx.width) or (c.height < ctx.height) do
    begin
      if (c.width < ctx.width) then UpsampleH(c);
//      if (ctx.error>OK) then Exit;
      if (c.height < ctx.height) then UpsampleV(c);
//      if (ctx.error>OK) then Exit;
    end;
    {$else}
    if ((c.width < ctx.width) or (c.height < ctx.height)) then
      Upsample(c);
    {$endif}

    if ((c.width < ctx.width) or (c.height < ctx.height)) then
      njThrow(InternalError);
    Inc(c);
  end;
  if (ctx.ncomp = 3) then
  begin
    // convert to RGB (and flip vertical)
    prgb := ctx.rgb;
    py  := ctx.comp[0].pixels + (ctx.height-1)*ctx.comp[0].stride;
    pcb := ctx.comp[1].pixels + (ctx.height-1)*ctx.comp[1].stride;
    pcr := ctx.comp[2].pixels + (ctx.height-1)*ctx.comp[2].stride;
    for yy := ctx.height downto 1 do
    begin
      for x := 0 to ctx.width-1 do
      begin
        y := py[x] shl 8;
        cb := pcb[x] - 128;
        cr := pcr[x] - 128;
        prgb^ := Clip(cshr((y            + 359 * cr + 128), 8)); Inc(prgb);
        prgb^ := Clip(cshr((y -  88 * cb - 183 * cr + 128), 8)); Inc(prgb);
        prgb^ := Clip(cshr((y + 454 * cb            + 128), 8)); Inc(prgb);
      end;
      py := py - ctx.comp[0].stride;
      pcb := pcb - ctx.comp[1].stride;
      pcr := pcr - ctx.comp[2].stride;
    end;
  end else if (ctx.comp[0].width <> ctx.comp[0].stride) then
  begin
    // grayscale -> only remove stride
    pin := @ctx.comp[0].pixels[ctx.comp[0].stride];
    pout := @ctx.comp[0].pixels[ctx.comp[0].width];
    for y := ctx.comp[0].height - 1 downto 1 do
    begin
      Move(pout^, pin^, ctx.comp[0].width);
      Inc(pin,ctx.comp[0].stride);
      Inc(pout,ctx.comp[0].width);
    end;
    ctx.comp[0].stride := ctx.comp[0].width;
  end;
end;

function TNjDecoder.Decode(jpeg : pointer; const size : integer) : boolean;
begin
  ctx.pos := jpeg;
  ctx.size := size and $7FFFFFFF;

  if (ctx.size < 2) then
    Exit(False);//return NJ_NO_JPEG;

  if (((ctx.pos[0] xor $FF)<>0) or ((ctx.pos[1] xor $D8)<>0)) then
    Exit(False);//return NJ_NO_JPEG;

  Skip(2);
  while ctx.error=OK do
  begin
    if ((ctx.size < 2) or (ctx.pos[0] <> $FF)) then
      Exit(False);//return NJ_SYNTAX_ERROR;
    Skip(2);
    case (ctx.pos[-1]) of
      $C0: DecodeSOF();
      $C4: DecodeDHT();
      $DB: DecodeDQT();
      $DD: DecodeDRI();
      $DA: DecodeScan();
      $FE: SkipMarker();
    else
      begin
        if ((ctx.pos[-1] and $F0) = $E0) then
          SkipMarker()
        else
          Exit(False);//return NJ_UNSUPPORTED;
      end;
    end;
  end;
  if (ctx.error<>Internal_Finished) then
    Exit(False);
  Convert();
  Result := True;
end;

//------------------------------------------------------------------------------
//
// TImgDecoder
//
//------------------------------------------------------------------------------

Constructor TImgDecoder.Create;
 begin
  FImgFormat   := ifUnknown;
  FImgWidth    := 0;
  FImgHeight   := 0;
  FImgPtr      := nil;
  FPixFormat   := pfUnknown;
  FJpegDecoder := nil;
 end;

Destructor TImgDecoder.Destroy;
 begin
  //
  If FImgPtr <> nil then FreeMem(FImgPtr);

  Case FImgFormat of
   ifPng  : ;
   ifJpeg : If FJpegDecoder <> nil then
             TImgDecoder(FJpegDecoder).Free;
  End;
  inherited;
 end;

Procedure TImgDecoder.Free;
 begin
  If Self <> nil then Destroy;
 end;

//
Function TImgDecoder.HasAlpha(ImgPtr : Pointer) : Boolean;
 Var
  Alpha : Boolean;
  i     : Integer;
 begin
  Alpha:=false;
  For i:=0 to (FImgWidth*FImgHeight)-1 do
   begin
    Alpha := Alpha or (byte(pansichar(FImgPtr)[(i*4)+3])<>255);
    if Alpha then
     begin
      Result := True;
      Exit;
     end;
   end;
  Result := Alpha;
 end;

//
Procedure TImgDecoder.SetAlpha(ImgPtr : Pointer; Alpha : Byte);
 Var
  AlphaD : DWord;
  ImgSrc : PDWord;
  i      : Integer;
 begin
  //
  If FImgWidth*FImgHeight = 0 then Exit;
  //
  AlphaD := Alpha;
  AlphaD := AlphaD shl 24;
  ImgSrc := ImgPtr;
  For i := 0 to FImgWidth*FImgHeight-1 do
   begin
    ImgSrc^ := ImgSrc^ or AlphaD;
    Inc(ImgPtr);
   end;
 end;

//
Function TImgDecoder.GetImgFormat(Stream : TMemoryStream) : TImgFormat;
 Const
  _cSigPng = $474E5089;
  _cSigJpg = $0000D8FF;
 Var
  Sig : DWord;
 begin
  Move(Stream.Memory^,Sig,4);
  Result := ifUnknown;
  If (Sig and _cSigPng) = _cSigPng then
   begin
    Result := ifPng;
    Exit;
   end;
  If (Sig and _cSigJpg) = _cSigJpg then
   begin
    Result := ifJpeg;
    Exit;
   end;
 end;

Function TImgDecoder.LoadFromFile(FileName : String) : Boolean;
 Var
  Stream : TMemoryStream;
  Jpeg   : TNJDecoder;
  Rst    : Boolean;
 begin
  //
  Result := false;
  If Not( FileExists(FileName) ) then Exit;
  //
  Stream := TMemoryStream.Create;
  Stream.LoadFromFile(FileName);
  If Stream.Size = 0 then
   begin
    jLog('Stream Size = 0');
    Stream.Free;
    Exit;
   end;
  //
  FImgFormat := GetImgFormat(Stream);
  Case FImgFormat of
   ifPng  : begin
             Rst := LoadPng(Stream.Memory,Stream.Size,FImgPtr,FImgWidth,FImgHeight);
             Stream.Free;
             If Not(Rst) then
              begin
               FreeMem(FImgPtr);
               FImgPtr    := nil;
               FImgWidth  := 0;
               FImgHeight := 0;
               Exit;
              end;
             //
             If Not(HasAlpha(FImgPtr)) then
              SetAlpha(FImgPtr,$FF);
             FPixFormat := pfRGBA;
             Result := true;
            end;
   ifJpeg : begin
             Jpeg := TNJDecoder.Create;
             Rst := Jpeg.Decode(Stream.Memory,Stream.Size);
             Stream.Free;
             If Not(Rst) then
              begin
               FreeMem(Jpeg.GetImage);
               FImgPtr      := nil;
               FImgWidth    := 0;
               FImgHeight   := 0;
               Jpeg.Free;
               FJpegDecoder := nil;
               Exit;
              end;
             //
             FImgWidth    := Jpeg.GetWidth;
             FImgHeight   := Jpeg.GetHeight;
             FImgPtr      := Jpeg.GetImage;
             FPixFormat   := pfRGB;
             FJpegDecoder := Jpeg;
             Result := true;
            end;
  End;
 end;


end.
