//
//   unzip single file interface for Delphi, FPC
//
//   http://blog.naver.com/simonsayz
//   simon,choi , ÃÖ¿ø½Ä¿Ë
//
//
//   Original : http://sageshome.net/oss/paszlib-sg.php
//               paszlib-sg-20030511.zip
//
//   Usages   :
//              Var
//               Stream : TMemoryStream;
//              begin
//               Stream := TMemoryStream.Create;
//               ZipExtract( 'ndkex01.zip','assets/readme.txt',Stream);
//               Stream.SaveToFile('d:\out.txt');
//              end;
//
//   History  :
//              2013.02.17 Started
//
//   Tested   : Android, PC  (FPC, Delphi)
//
unit And_lib_Unzip;

interface

{$DEFINE patch112}        { apply patch from the zlib home page }
{ -------------------------------------------------------------------- }

Uses
 Classes,SysUtils;

Type
  voidp  = pointer;
  zipFile = voidp;
  unzFile = voidp;

  int    = integer;
  uInt   = Cardinal;
  unsigned = uInt;
  uLong  = Cardinal;
  Long   = longint;
  uLongf = uLong;

  z_off_t = long;
//------------------------------------------------------------------------------
// from unzip
//------------------------------------------------------------------------------

const
   Z_ERRNO         = (-1);

  UNZ_OK = (0);
  UNZ_END_OF_LIST_OF_FILE = (-100);
  UNZ_ERRNO = (Z_ERRNO);
  UNZ_EOF = (0);
  UNZ_PARAMERROR = (-102);
  UNZ_BADZIPFILE = (-103);
  UNZ_INTERNALERROR = (-104);
  UNZ_CRCERROR = (-105);

 { unz_global_info structure contain global data about the ZIPfile
  These data comes from the end of central dir }
type
  unz_global_info = record
    number_entry : uLong;   { total number of entries in
                              the central dir on this disk }
    size_comment : uLong;   { size of the global comment of the zipfile }
  end;


{ unz_file_info contain information about a file in the zipfile }
type
  tm_zip = record
     tm_sec : uInt;            { seconds after the minute - [0,59] }
     tm_min : uInt;            { minutes after the hour - [0,59] }
     tm_hour : uInt;           { hours since midnight - [0,23] }
     tm_mday : uInt;           { day of the month - [1,31] }
     tm_mon : uInt;            { months since January - [0,11] }
     tm_year : uInt;           { years - [1980..2044] }
  end;

 tm_unz = tm_zip;

  unz_file_info = record
    version : uLong;              { version made by                 2 bytes }
    version_needed : uLong;       { version needed to extract       2 bytes }
    flag : uLong;                 { general purpose bit flag        2 bytes }
    compression_method : uLong;   { compression method              2 bytes }
    dosDate : uLong;              { last mod file date in Dos fmt   4 bytes }
    crc : uLong;                  { crc-32                          4 bytes }
    compressed_size : uLong;      { compressed size                 4 bytes }
    uncompressed_size : uLong;    { uncompressed size               4 bytes }
    size_filename : uLong;        { filename length                 2 bytes }
    size_file_extra : uLong;      { extra field length              2 bytes }
    size_file_comment : uLong;    { file comment length             2 bytes }

    disk_num_start : uLong;       { disk number start               2 bytes }
    internal_fa : uLong;          { internal file attributes        2 bytes }
    external_fa : uLong;          { external file attributes        4 bytes }

    tmu_date : tm_unz;
  end;
  unz_file_info_ptr = ^unz_file_info;


function unzClose (afile : unzFile) : int;

function unzGetGlobalInfo (afile : unzFile;
                           var pglobal_info : unz_global_info) : int;


function unzGetGlobalComment (afile : unzFile;
                              szComment : PChar;
            uSizeBuf : uLong) : int;

function unzGoToFirstFile(afile : unzFile) : int;
function unzGoToNextFile(afile : unzFile) : int;
function unzLocateFile(afile : unzFile;
                       const szFileName : PChar;
           iCaseSensitivity : int) : int; { ZEXPORT }
function unzGetCurrentFileInfo(afile : unzFile;
                               pfile_info : unz_file_info_ptr;
             szFileName : PChar;
             fileNameBufferSize : uLong;
             extraField : voidp;
             extraFieldBufferSize : uLong;
             szComment : PChar;
             commentBufferSize : uLong) : int; { ZEXPORT }
function unzOpenCurrentFile(afile : unzFile) : int; { ZEXPORT }
function unzCloseCurrentFile(afile : unzFile) : int; { ZEXPORT }

function unzReadCurrentFile(afile : unzFile;
                            buf : voidp;
          len : unsigned) : int; { ZEXPORT }


function unztell(afile : unzFile) : z_off_t;


function unzeof(afile : unzFile) : int;


function unzGetLocalExtrafield (afile : unzFile;
                                buf : voidp;
                                len : unsigned) : int;

{ ----------------------------------------------------------------- }

function unzOpen (const path : PChar) : unzFile; { ZEXPORT }
function unzExtractFile(uf     : unzFile;
                        inFile : String;
                        stream : TStream) : Boolean;

Function ZipExtract( zipfile, infile : string; outfile : TStream ) : Boolean;


implementation

type
  {Byte   = usigned char;  8 bits}
  Bytef  = byte;
  charf  = byte;
  intf   = int;
  uIntf  = uInt;

  voidpf = voidp;
  pBytef = ^Bytef;
  pIntf  = ^intf;
  puIntf = ^uIntf;
  puLong = ^uLongf;

  ptr2int = uInt;
{ a pointer to integer casting is used to do pointer arithmetic.
  ptr2int must be an integer type and sizeof(ptr2int) must be less
  than sizeof(pointer) - Nomssi }

const
  MaxMemBlock = MaxInt;

type
  zByteArray = array[0..(MaxMemBlock div SizeOf(Bytef))-1] of Bytef;
  pzByteArray = ^zByteArray;
type
  zIntfArray = array[0..(MaxMemBlock div SizeOf(Intf))-1] of Intf;
  pzIntfArray = ^zIntfArray;
type
  zuIntArray = array[0..(MaxMemBlock div SizeOf(uInt))-1] of uInt;
  PuIntArray = ^zuIntArray;

{ Type declarations - only for deflate }

type
  uch  = Byte;
  uchf = uch; { FAR }
  ush  = Word;
  ushf = ush;
  ulg  = LongInt;


  pcharf = ^charf;
  puchf = ^uchf;
  pushf = ^ushf;

type
  zuchfArray = zByteArray;
  puchfArray = ^zuchfArray;
type
  zushfArray = array[0..(MaxMemBlock div SizeOf(ushf))-1] of ushf;
  pushfArray = ^zushfArray;

{ zconf.h -- configuration of the zlib compression library }
{ zutil.c -- target dependent utility functions for the compression library }

{ Maximum value for memLevel in deflateInit2 }
const
  MAX_MEM_LEVEL = 9;
  DEF_MEM_LEVEL = 8; { if MAX_MEM_LEVEL > 8 }

{ Maximum value for windowBits in deflateInit2 and inflateInit2 }
const
  MAX_WBITS = 15; { 32K LZ77 window }

{ default windowBits for decompression. MAX_WBITS is for compression only }
const
  DEF_WBITS = MAX_WBITS;


{ Huffman code lookup table entry--this entry is four bytes for machines
  that have 16-bit pointers (e.g. PC's in the small or medium model). }

type
  pInflate_huft = ^inflate_huft;
  inflate_huft = Record
    Exop,             { number of extra bits or operation }
    bits : Byte;      { number of bits in this code or subcode }
    {pad : uInt;}       { pad structure to a power of 2 (4 bytes for }
                      {  16-bit, 8 bytes for 32-bit int's) }
    base : uInt;      { literal, length base, or distance base }
                      { or table offset }
  End;

type
  huft_field = Array[0..(MaxMemBlock div SizeOf(inflate_huft))-1] of inflate_huft;
  huft_ptr = ^huft_field;
type
  ppInflate_huft = ^pInflate_huft;

type
  inflate_codes_mode = ( { waiting for "i:"=input, "o:"=output, "x:"=nothing }
        START,    { x: set up for LEN }
        LEN,      { i: get length/literal/eob next }
        LENEXT,   { i: getting length extra (have base) }
        DIST,     { i: get distance next }
        DISTEXT,  { i: getting distance extra }
        COPY,     { o: copying bytes in window, waiting for space }
        LIT,      { o: got literal, waiting for output space }
        WASH,     { o: got eob, possibly still output waiting }
        ZEND,     { x: got eob and all data flushed }
        BADCODE); { x: got error }

{ inflate codes private state }
type
  pInflate_codes_state = ^inflate_codes_state;
  inflate_codes_state = record

    mode : inflate_codes_mode;        { current inflate_codes mode }

    { mode dependent information }
    len : uInt;
    sub : record                      { submode }
      Case Byte of
      0:(code : record                { if LEN or DIST, where in tree }
          tree : pInflate_huft;       { pointer into tree }
          need : uInt;                { bits needed }
         end);
      1:(lit : uInt);                 { if LIT, literal }
      2:(copy: record                 { if EXT or COPY, where and how much }
           get : uInt;                { bits to get for extra }
           dist : uInt;               { distance back to copy from }
         end);
    end;

    { mode independent information }
    lbits : Byte;                     { ltree bits decoded per branch }
    dbits : Byte;                     { dtree bits decoder per branch }
    ltree : pInflate_huft;            { literal/length/eob tree }
    dtree : pInflate_huft;            { distance tree }
  end;

type
  check_func = function(check : uLong;
                        buf : pBytef;
                        {const buf : array of byte;}
	                len : uInt) : uLong;
type
  inflate_block_mode =
     (ZTYPE,    { get type bits (3, including end bit) }
      LENS,     { get lengths for stored }
      STORED,   { processing stored block }
      TABLE,    { get table lengths }
      BTREE,    { get bit lengths tree for a dynamic block }
      DTREE,    { get length, distance trees for a dynamic block }
      CODES,    { processing fixed or dynamic block }
      DRY,      { output remaining window bytes }
      BLKDONE,  { finished last block, done }
      BLKBAD);  { got a data error--stuck here }

type
  pInflate_blocks_state = ^inflate_blocks_state;

{ inflate blocks semi-private state }
  inflate_blocks_state = record

    mode : inflate_block_mode;     { current inflate_block mode }

    { mode dependent information }
    sub : record                  { submode }
    case Byte of
    0:(left : uInt);              { if STORED, bytes left to copy }
    1:(trees : record             { if DTREE, decoding info for trees }
        table : uInt;               { table lengths (14 bits) }
        index : uInt;               { index into blens (or border) }
        blens : PuIntArray;         { bit lengths of codes }
        bb : uInt;                  { bit length tree depth }
        tb : pInflate_huft;         { bit length decoding tree }
      end);
    2:(decode : record            { if CODES, current state }
        tl : pInflate_huft;
        td : pInflate_huft;         { trees to free }
        codes : pInflate_codes_state;
      end);
    end;
    last : boolean;               { true if this block is the last block }

    { mode independent information }
    bitk : uInt;            { bits in bit buffer }
    bitb : uLong;           { bit buffer }
    hufts : huft_ptr; {pInflate_huft;}  { single malloc for tree space }
    window : pBytef;        { sliding window }
    zend : pBytef;          { one byte after sliding window }
    read : pBytef;          { window read pointer }
    write : pBytef;         { window write pointer }
    checkfn : check_func;   { check function }
    check : uLong;          { check on output }
  end;

type
  inflate_mode = (
      METHOD,   { waiting for method byte }
      FLAG,     { waiting for flag byte }
      DICT4,    { four dictionary check bytes to go }
      DICT3,    { three dictionary check bytes to go }
      DICT2,    { two dictionary check bytes to go }
      DICT1,    { one dictionary check byte to go }
      DICT0,    { waiting for inflateSetDictionary }
      BLOCKS,   { decompressing blocks }
      CHECK4,   { four check bytes to go }
      CHECK3,   { three check bytes to go }
      CHECK2,   { two check bytes to go }
      CHECK1,   { one check byte to go }
      DONE,     { finished check, done }
      BAD);     { got an error--stay here }

{ inflate private state }
type
  pInternal_state = ^internal_state; { or point to a deflate_state record }
  internal_state = record

     mode : inflate_mode;  { current inflate mode }

     { mode dependent information }
     sub : record          { submode }
       case byte of
       0:(method : uInt);  { if FLAGS, method byte }
       1:(check : record   { if CHECK, check values to compare }
           was : uLong;        { computed check value }
           need : uLong;       { stream check value }
          end);
       2:(marker : uInt);  { if BAD, inflateSync's marker bytes count }
     end;

     { mode independent information }
     nowrap : boolean;      { flag for no wrapper }
     wbits : uInt;          { log2(window size)  (8..15, defaults to 15) }
     blocks : pInflate_blocks_state;    { current inflate_blocks state }
   end;

type
  alloc_func = function(opaque : voidpf; items : uInt; size : uInt) : voidpf;
  free_func = procedure(opaque : voidpf; address : voidpf);

type
  z_streamp = ^z_stream;
  z_stream = record
    next_in : pBytef;     { next input byte }
    avail_in : uInt;      { number of bytes available at next_in }
    total_in : uLong;     { total nb of input bytes read so far }

    next_out : pBytef;    { next output byte should be put there }
    avail_out : uInt;     { remaining free space at next_out }
    total_out : uLong;    { total nb of bytes output so far }

    msg : string[255];         { last error message, '' if no error }
    state : pInternal_state; { not visible by applications }

    zalloc : alloc_func;  { used to allocate the internal state }
    zfree : free_func;    { used to free the internal state }
    opaque : voidpf;      { private data object passed to zalloc and zfree }

    data_type : int;      { best guess about the data type: ascii or binary }
    adler : uLong;        { adler32 value of the uncompressed data }
    reserved : uLong;     { reserved for future use }
  end;


{  The application must update next_in and avail_in when avail_in has
   dropped to zero. It must update next_out and avail_out when avail_out
   has dropped to zero. The application must initialize zalloc, zfree and
   opaque before calling the init function. All other fields are set by the
   compression library and must not be updated by the application.

   The opaque value provided by the application will be passed as the first
   parameter for calls of zalloc and zfree. This can be useful for custom
   memory management. The compression library attaches no meaning to the
   opaque value.

   zalloc must return Z_NULL if there is not enough memory for the object.
   On 16-bit systems, the functions zalloc and zfree must be able to allocate
   exactly 65536 bytes, but will not be required to allocate more than this
   if the symbol MAXSEG_64K is defined (see zconf.h). WARNING: On MSDOS,
   pointers returned by zalloc for objects of exactly 65536 bytes *must*
   have their offset normalized to zero. The default allocation function
   provided by this library ensures this (see zutil.c). To reduce memory
   requirements and avoid any allocation of 64K objects, at the expense of
   compression ratio, compile the library with -DMAX_WBITS=14 (see zconf.h).

   The fields total_in and total_out can be used for statistics or
   progress reports. After compression, total_in holds the total size of
   the uncompressed data and may be saved for use in the decompressor
   (particularly if the decompressor wants to decompress everything in
   a single step). }

const  { constants }
   Z_NO_FLUSH      = 0;
   Z_PARTIAL_FLUSH = 1;
   Z_SYNC_FLUSH    = 2;
   Z_FULL_FLUSH    = 3;
   Z_FINISH        = 4;
{ Allowed flush values; see deflate() below for details }

   Z_OK            = 0;
   Z_STREAM_END    = 1;
   Z_NEED_DICT     = 2;
   Z_STREAM_ERROR  = (-2);
   Z_DATA_ERROR    = (-3);
   Z_MEM_ERROR     = (-4);
   Z_BUF_ERROR     = (-5);
   Z_VERSION_ERROR = (-6);
{ Return codes for the compression/decompression functions. Negative
  values are errors, positive values are used for special but normal events.}

   Z_NO_COMPRESSION         = 0;
   Z_BEST_SPEED             = 1;
   Z_BEST_COMPRESSION       = 9;
   Z_DEFAULT_COMPRESSION    = (-1);
{ compression levels }

   Z_FILTERED            = 1;
   Z_HUFFMAN_ONLY        = 2;
   Z_DEFAULT_STRATEGY    = 0;
{ compression strategy; see deflateInit2() below for details }

   Z_BINARY   = 0;
   Z_ASCII    = 1;
   Z_UNKNOWN  = 2;
{ Possible values of the data_type field }

   Z_DEFLATED   = 8;
{ The deflate compression method (the only one supported in this version) }

   Z_NULL  = NIL;  { for initializing zalloc, zfree, opaque }

  {$IFDEF GZIO}
var
  errno : int;
  {$ENDIF}

        { common constants }


{ The three kinds of block type }
const
  STORED_BLOCK = 0;
  STATIC_TREES = 1;
  DYN_TREES = 2;
{ The minimum and maximum match lengths }
const
  MIN_MATCH = 3;
  MAX_MATCH = 258;

const
  PRESET_DICT = $20; { preset dictionary flag in zlib header }

const
  ZLIB_VERSION : string[10] = '1.1.2';

const
  z_errbase = Z_NEED_DICT;
  z_errmsg : Array[0..9] of string[21] = { indexed by 2-zlib_error }
           ('need dictionary',     { Z_NEED_DICT       2  }
            'stream end',          { Z_STREAM_END      1  }
            '',                    { Z_OK              0  }
            'file error',          { Z_ERRNO         (-1) }
            'stream error',        { Z_STREAM_ERROR  (-2) }
            'data error',          { Z_DATA_ERROR    (-3) }
            'insufficient memory', { Z_MEM_ERROR     (-4) }
            'buffer error',        { Z_BUF_ERROR     (-5) }
            'incompatible version',{ Z_VERSION_ERROR (-6) }
            '');
const
  z_verbose : int = 1;

//------------------------------------------------------------------------------
// from zutil
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// adler
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// from CRC
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// from infutil
//------------------------------------------------------------------------------

// And'ing with mask[n] masks the lower n bits
const
  inflate_mask : array[0..17-1] of uInt = (
    $0000,
    $0001, $0003, $0007, $000f, $001f, $003f, $007f, $00ff,
    $01ff, $03ff, $07ff, $0fff, $1fff, $3fff, $7fff, $ffff);


{ Maximum size of dynamic tree.  The maximum found in a long but non-
  exhaustive search was 1004 huft structures (850 for length/literals
  and 154 for distances, the latter actually the result of an
  exhaustive search).  The actual maximum is not known, but the
  value below is more than safe. }
const
  MANY = 1440;

//------------------------------------------------------------------------------
// from inftrees
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// from infBlock
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// from zinflate
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// from ziputils
//------------------------------------------------------------------------------
type
  FILEptr = TFileStream;
  seek_mode = (SEEK_SET, SEEK_CUR, SEEK_END);
  open_mode = (fopenread, fopenwrite, fappendwrite);

const
  Z_BUFSIZE = (16384);
  Z_MAXFILENAMEINZIP = (256);

const
  CENTRALHEADERMAGIC = $02014b50;

const
  SIZECENTRALDIRITEM = $2e;
  SIZEZIPLOCALHEADER = $1e;

const
  Paszip_copyright : PChar = ' Paszip Copyright 2000 Jacques Nomssi Nzali ';




//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------


// from zutil
type
  LH = record
    L, H : word;
  end;

procedure zmemcpy(destp : pBytef; sourcep : pBytef; len : uInt);
begin
  Move(sourcep^, destp^, len);
end;

function zmemcmp(s1p, s2p : pBytef; len : uInt) : int;
var
  j : uInt;
  source,
  dest : pBytef;
begin
  source := s1p;
  dest := s2p;
  for j := 0 to pred(len) do
  begin
    if (source^ <> dest^) then
    begin
      zmemcmp := 2*Ord(source^ > dest^)-1;
      exit;
    end;
    Inc(source);
    Inc(dest);
  end;
  zmemcmp := 0;
end;

procedure zmemzero(destp : pBytef; len : uInt);
begin
  FillChar(destp^, len, 0);
end;

procedure zcfree(opaque : voidpf; ptr : voidpf);
 {$IFDEF FPC}
var
  memsize : uint;
 {$ENDIF}
 begin
  {$IFDEF FPC}
   Dec(puIntf(ptr));
   memsize := puIntf(ptr)^;
   FreeMem(ptr, memsize+SizeOf(uInt));
  {$ELSE}
   FreeMem(ptr);  { Delphi 2,3,4 }
  {$ENDIF}
 end;

function zcalloc (opaque : voidpf; items : uInt; size : uInt) : voidpf;
var
  p : voidpf;
  memsize : uLong;
begin
  memsize := uLong(items) * size;
  {$IFDEF FPC}
   GetMem(p, memsize+SizeOf(uInt));
   puIntf(p)^:= memsize;
   Inc(puIntf(p));
  {$ELSE}
   GetMem(p, memsize);  { Delphi: p := AllocMem(memsize); }
  {$ENDIF}
  zcalloc := p;
end;


// from gzlib
function zError(err : int) : string;
begin
  zError := z_errmsg[Z_NEED_DICT-err];
end;

function zlibVersion : string;
begin
  zlibVersion := ZLIB_VERSION;
end;

procedure z_error (m : string);
begin
  WriteLn(output, m);
  Write('Zlib - Halt...');
  ReadLn;
  Halt(1);
end;

procedure Assert(cond : boolean; msg : string);
begin
  if not cond then
    z_error(msg);
end;

procedure Trace(x : string);
begin
  WriteLn(x);
end;

procedure Tracev(x : string);
begin
 if (z_verbose>0) then
   WriteLn(x);
end;

procedure Tracevv(x : string);
begin
  if (z_verbose>1) then
    WriteLn(x);
end;

procedure Tracevvv(x : string);
begin
  if (z_verbose>2) then
    WriteLn(x);
end;

procedure Tracec(c : boolean; x : string);
begin
  if (z_verbose>0) and (c) then
    WriteLn(x);
end;

procedure Tracecv(c : boolean; x : string);
begin
  if (z_verbose>1) and c then
    WriteLn(x);
end;

function ZALLOC (var strm : z_stream; items : uInt; size : uInt) : voidpf;
begin
  ZALLOC := strm.zalloc(strm.opaque, items, size);
end;

procedure ZFREE (var strm : z_stream; ptr : voidpf);
begin
  strm.zfree(strm.opaque, ptr);
end;

procedure TRY_FREE (var strm : z_stream; ptr : voidpf);
begin
  {if @strm <> Z_NULL then}
    strm.zfree(strm.opaque, ptr);
end;

//
const
  BASE = uLong(65521); { largest prime smaller than 65536 }
  {NMAX = 5552; original code with unsigned 32 bit integer }
  { NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 }
  NMAX = 3854;        { code with signed 32 bit integer }
  { NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^31-1 }
  { The penalty is the time loss in the extra MOD-calls. }


{ ========================================================================= }

function adler32(adler : uLong; buf : pBytef; len : uInt) : uLong;
var
  s1, s2 : uLong;
  k : int;
begin
  s1 := adler and $ffff;
  s2 := (adler shr 16) and $ffff;

  if not Assigned(buf) then
  begin
    adler32 := uLong(1);
    exit;
  end;

  while (len > 0) do
  begin
    if len < NMAX then
      k := len
    else
      k := NMAX;
    Dec(len, k);
    while (k > 0) do
    begin
      Inc(s1, buf^);
      Inc(s2, s1);
      Inc(buf);
      Dec(k);
    end;
    s1 := s1 mod BASE;
    s2 := s2 mod BASE;
  end;
  adler32 := (s2 shl 16) or s1;
end;

// from CRC32
{ ========================================================================
  Table of CRC-32's of all single-byte values (made by make_crc_table) }

{local}
const
  crc_table : array[0..255] of uLongf = (
  $00000000, $77073096, $ee0e612c, $990951ba, $076dc419,
  $706af48f, $e963a535, $9e6495a3, $0edb8832, $79dcb8a4,
  $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07,
  $90bf1d91, $1db71064, $6ab020f2, $f3b97148, $84be41de,
  $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7, $136c9856,
  $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9,
  $fa0f3d63, $8d080df5, $3b6e20c8, $4c69105e, $d56041e4,
  $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
  $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3,
  $45df5c75, $dcd60dcf, $abd13d59, $26d930ac, $51de003a,
  $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599,
  $b8bda50f, $2802b89e, $5f058808, $c60cd9b2, $b10be924,
  $2f6f7c87, $58684c11, $c1611dab, $b6662d3d, $76dc4190,
  $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f,
  $9fbfe4a5, $e8b8d433, $7807c9a2, $0f00f934, $9609a88e,
  $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
  $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed,
  $1b01a57b, $8208f4c1, $f50fc457, $65b0d9c6, $12b7e950,
  $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3,
  $fbd44c65, $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2,
  $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb, $4369e96a,
  $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5,
  $aa0a4c5f, $dd0d7cc9, $5005713c, $270241aa, $be0b1010,
  $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
  $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17,
  $2eb40d81, $b7bd5c3b, $c0ba6cad, $edb88320, $9abfb3b6,
  $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615,
  $73dc1683, $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
  $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1, $f00f9344,
  $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb,
  $196c3671, $6e6b06e7, $fed41b76, $89d32be0, $10da7a5a,
  $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
  $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1,
  $a6bc5767, $3fb506dd, $48b2364b, $d80d2bda, $af0a1b4c,
  $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef,
  $4669be79, $cb61b38c, $bc66831a, $256fd2a0, $5268e236,
  $cc0c7795, $bb0b4703, $220216b9, $5505262f, $c5ba3bbe,
  $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31,
  $2cd99e8b, $5bdeae1d, $9b64c2b0, $ec63f226, $756aa39c,
  $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
  $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b,
  $e5d5be0d, $7cdcefb7, $0bdbdf21, $86d3d2d4, $f1d4e242,
  $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1,
  $18b74777, $88085ae6, $ff0f6a70, $66063bca, $11010b5c,
  $8f659eff, $f862ae69, $616bffd3, $166ccf45, $a00ae278,
  $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7,
  $4969474d, $3e6e77db, $aed16a4a, $d9d65adc, $40df0b66,
  $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
  $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605,
  $cdd70693, $54de5729, $23d967bf, $b3667a2e, $c4614ab8,
  $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b,
  $2d02ef8d);

{ =========================================================================
  This function can be used by asm versions of crc32() }

function get_crc_table : {const} puLong;
begin
  get_crc_table :=  {const} puLong(@crc_table);
end;

{ ========================================================================= }

function crc32 (crc : uLong; buf : pBytef; len : uInt): uLong;
begin
  if (buf = Z_NULL) then
    crc32 := Long(0)
  else
  begin
    crc := crc xor uLong($ffffffff);
    while (len >= 8) do
    begin
      {DO8(buf)}
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);

      Dec(len, 8);
    end;
    if (len <> 0) then
    repeat
      {DO1(buf)}
      crc := crc_table[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);

      Dec(len);
    until (len = 0);
    crc32 := crc xor uLong($ffffffff);
  end;
end;

// fomr infutil
{ macros for bit input with no checking and for returning unused bytes }
procedure GRABBITS(j : int);
begin
  {while (k < j) do
  begin
    Dec(z^.avail_in);
    Inc(z^.total_in);
    b := b or (uLong(z^.next_in^) shl k);
    Inc(z^.next_in);
    Inc(k, 8);
  end;}
end;

procedure DUMPBITS(j : int);
begin
  {b := b shr j;
  Dec(k, j);}
end;

procedure NEEDBITS(j : int);
begin
 (*
          while (k < j) do
          begin
            {NEEDBYTE;}
            if (n <> 0) then
              r :=Z_OK
            else
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, LongInt(p)-LongInt(z.next_in));
              z.next_in := p;
              s.write := q;
              result := inflate_flush(s,z,r);
              exit;
            end;
            Dec(n);
            b := b or (uLong(p^) shl k);
            Inc(p);
            Inc(k, 8);
          end;
 *)
end;

procedure NEEDOUT;
begin
 (*
  if (m = 0) then
  begin
    {WRAP}
    if (q = s.zend) and (s.read <> s.window) then
    begin
      q := s.window;
      if LongInt(q) < LongInt(s.read) then
        m := uInt(LongInt(s.read)-LongInt(q)-1)
      else
        m := uInt(LongInt(s.zend)-LongInt(q));
    end;

    if (m = 0) then
    begin
      {FLUSH}
      s.write := q;
      r := inflate_flush(s,z,r);
      q := s.write;
      if LongInt(q) < LongInt(s.read) then
        m := uInt(LongInt(s.read)-LongInt(q)-1)
      else
        m := uInt(LongInt(s.zend)-LongInt(q));

      {WRAP}
      if (q = s.zend) and (s.read <> s.window) then
      begin
        q := s.window;
        if LongInt(q) < LongInt(s.read) then
          m := uInt(LongInt(s.read)-LongInt(q)-1)
        else
          m := uInt(LongInt(s.zend)-LongInt(q));
      end;

      if (m = 0) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, LongInt(p)-LongInt(z.next_in));
        z.next_in := p;
        s.write := q;
        result := inflate_flush(s,z,r);
        exit;
      end;
    end;
  end;
  r := Z_OK;
 *)
end;

{ copy as much as possible from the sliding window to the output area }
function inflate_flush(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;
var
  n : uInt;
  p : pBytef;
  q : pBytef;
begin
  { local copies of source and destination pointers }
  p := z.next_out;
  q := s.read;

  { compute number of bytes to copy as far as end of window }
  if ptr2int(q) <= ptr2int(s.write) then
    n := uInt(ptr2int(s.write) - ptr2int(q))
  else
    n := uInt(ptr2int(s.zend) - ptr2int(q));
  if (n > z.avail_out) then
    n := z.avail_out;
  if (n <> 0) and (r = Z_BUF_ERROR) then
    r := Z_OK;

  { update counters }
  Dec(z.avail_out, n);
  Inc(z.total_out, n);


  { update check information }
  if Assigned(s.checkfn) then
  begin
    s.check := s.checkfn(s.check, q, n);
    z.adler := s.check;
  end;

  { copy as far as end of window }
  zmemcpy(p, q, n);
  Inc(p, n);
  Inc(q, n);

  { see if more to copy at beginning of window }
  if (q = s.zend) then
  begin
    { wrap pointers }
    q := s.window;
    if (s.write = s.zend) then
      s.write := s.window;

    { compute bytes to copy }
    n := uInt(ptr2int(s.write) - ptr2int(q));
    if (n > z.avail_out) then
      n := z.avail_out;
    if (n <> 0) and (r = Z_BUF_ERROR) then
      r := Z_OK;

    { update counters }
    Dec( z.avail_out, n);
    Inc( z.total_out, n);

    { update check information }
    if Assigned(s.checkfn) then
    begin
      s.check := s.checkfn(s.check, q, n);
      z.adler := s.check;
    end;

    { copy }
    zmemcpy(p, q, n);
    Inc(p, n);
    Inc(q, n);
  end;


  { update pointers }
  z.next_out := p;
  s.read := q;

  { done }
  inflate_flush := r;
end;

// from inftree
const
 inflate_copyright = 'inflate 1.1.2 Copyright 1995-1998 Mark Adler';

{
  If you use the zlib library in a product, an acknowledgment is welcome
  in the documentation of your product. If for some reason you cannot
  include such an acknowledgment, I would appreciate that you keep this
  copyright string in the executable of your product.
}


const
{ Tables for deflate from PKZIP's appnote.txt. }
  cplens : Array [0..30] Of uInt  { Copy lengths for literal codes 257..285 }
     = (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);
        { actually lengths - 2; also see note #13 above about 258 }

  invalid_code = 112;

  cplext : Array [0..30] Of uInt  { Extra bits for literal codes 257..285 }
     = (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, invalid_code, invalid_code);

  cpdist : Array [0..29] Of uInt { Copy offsets for distance codes 0..29 }
     = (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577);

  cpdext : Array [0..29] Of uInt { Extra bits for distance codes }
     = (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13);

{  Huffman code decoding is performed using a multi-level table lookup.
   The fastest way to decode is to simply build a lookup table whose
   size is determined by the longest code.  However, the time it takes
   to build this table can also be a factor if the data being decoded
   is not very long.  The most common codes are necessarily the
   shortest codes, so those codes dominate the decoding time, and hence
   the speed.  The idea is you can have a shorter table that decodes the
   shorter, more probable codes, and then point to subsidiary tables for
   the longer codes.  The time it costs to decode the longer codes is
   then traded against the time it takes to make longer tables.

   This results of this trade are in the variables lbits and dbits
   below.  lbits is the number of bits the first level table for literal/
   length codes can decode in one step, and dbits is the same thing for
   the distance codes.  Subsequent tables are also less than or equal to
   those sizes.  These values may be adjusted either when all of the
   codes are shorter than that, in which case the longest code length in
   bits is used, or when the shortest code is *longer* than the requested
   table size, in which case the length of the shortest code in bits is
   used.

   There are two different values for the two tables, since they code a
   different number of possibilities each.  The literal/length table
   codes 286 possible values, or in a flat code, a little over eight
   bits.  The distance table codes 30 possible values, or a little less
   than five bits, flat.  The optimum values for speed end up being
   about one bit more than those, so lbits is 8+1 and dbits is 5+1.
   The optimum values may differ though from machine to machine, and
   possibly even between compilers.  Your mileage may vary. }


{ If BMAX needs to be larger than 16, then h and x[] should be uLong. }
const
  BMAX = 15;         { maximum bit length of any code }

{$DEFINE USE_PTR}

function huft_build(
var b : array of uIntf;    { code lengths in bits (all assumed <= BMAX) }
    n : uInt;              { number of codes (assumed <= N_MAX) }
    s : uInt;              { number of simple-valued codes (0..s-1) }
const d : array of uIntf;  { list of base values for non-simple codes }
{ array of word }
const e : array of uIntf;  { list of extra bits for non-simple codes }
{ array of byte }
  t : ppInflate_huft;     { result: starting table }
var m : uIntf;             { maximum lookup bits, returns actual }
var hp : array of inflate_huft;  { space for trees }
var hn : uInt;             { hufts used in space }
var v : array of uIntf     { working area: values in order of bit length }
   ) : int;
{ Given a list of code lengths and a maximum table size, make a set of
  tables to decode that set of codes.  Return Z_OK on success, Z_BUF_ERROR
  if the given code set is incomplete (the tables are still built in this
  case), Z_DATA_ERROR if the input is invalid (an over-subscribed set of
  lengths), or Z_MEM_ERROR if not enough memory. }
Var
  a : uInt;                     { counter for codes of length k }
  c : Array [0..BMAX] Of uInt;  { bit length count table }
  f : uInt;                     { i repeats in table every f entries }
  g : int;                      { maximum code length }
  h : int;                      { table level }
  i : uInt;  {register}         { counter, current code }
  j : uInt;  {register}         { counter }
  k : Int;   {register}         { number of bits in current code }
  l : int;      { bits per table (returned in m) }
  mask : uInt;                  { (1 shl w) - 1, to avoid cc -O bug on HP }
  p : ^uIntf; {register}        { pointer into c[], b[], or v[] }
  q : pInflate_huft;            { points to current table }
  r : inflate_huft;             { table entry for structure assignment }
  u : Array [0..BMAX-1] Of pInflate_huft; { table stack }
  w : int;   {register}         { bits before this table = (l*h) }
  x : Array [0..BMAX] Of uInt;  { bit offsets, then code stack }
  {$IFDEF USE_PTR}
  xp : puIntf;                  { pointer into x }
  {$ELSE}
  xp : uInt;
  {$ENDIF}
  y : int;                      { number of dummy codes added }
  z : uInt;                     { number of entries in current table }
Begin
  { Generate counts for each bit length }
  FillChar(c,SizeOf(c),0) ;     { clear c[] }

  for i := 0 to n-1 do
    Inc (c[b[i]]);              { assume all entries <= BMAX }

  If (c[0] = n) Then            { null input--all zero length codes }
  Begin
    t^ := pInflate_huft(NIL);
    m := 0 ;
    huft_build := Z_OK ;
    Exit;
  End ;

  { Find minimum and maximum length, bound [m] by those }
  l := m;
  for j:=1 To BMAX do
    if (c[j] <> 0) then
      break;
  k := j ;                      { minimum code length }
  if (uInt(l) < j) then
    l := j;
  for i := BMAX downto 1 do
    if (c[i] <> 0) then
      break ;
  g := i ;                      { maximum code length }
  if (uInt(l) > i) then
     l := i;
  m := l;

  { Adjust last length count to fill out codes, if needed }
  y := 1 shl j ;
  while (j < i) do
  begin
    Dec(y, c[j]) ;
    if (y < 0) then
    begin
      huft_build := Z_DATA_ERROR;   { bad input: more codes than bits }
      exit;
    end ;
    Inc(j) ;
    y := y shl 1
  end;
  Dec (y, c[i]) ;
  if (y < 0) then
  begin
    huft_build := Z_DATA_ERROR;     { bad input: more codes than bits }
    exit;
  end;
  Inc(c[i], y);

  { Generate starting offsets into the value table FOR each length }
  {$IFDEF USE_PTR}
  x[1] := 0;
  j := 0;

  p := @c[1];
  xp := @x[2];

  dec(i);               { note that i = g from above }
  WHILE (i > 0) DO
  BEGIN
    inc(j, p^);
    xp^ := j;
    inc(p);
    inc(xp);
    dec(i);
  END;
  {$ELSE}
  x[1] := 0;
  j := 0 ;
  for i := 1 to g do
  begin
    x[i] := j;
    Inc(j, c[i]);
  end;
  {$ENDIF}

  { Make a table of values in order of bit lengths }
  for i := 0 to n-1 do
  begin
    j := b[i];
    if (j <> 0) then
    begin
      v[ x[j] ] := i;
      Inc(x[j]);
    end;
  end;
  n := x[g];                     { set n to length of v }

  { Generate the Huffman codes and for each, make the table entries }
  i := 0 ;
  x[0] := 0 ;                   { first Huffman code is zero }
  p := Addr(v) ;                { grab values in bit order }
  h := -1 ;                     { no tables yet--level -1 }
  w := -l ;                     { bits decoded = (l*h) }

  u[0] := pInflate_huft(NIL);   { just to keep compilers happy }
  q := pInflate_huft(NIL);      { ditto }
  z := 0 ;                      { ditto }

  { go through the bit lengths (k already is bits in shortest code) }
  while (k <= g) Do
  begin
    a := c[k] ;
    while (a<>0) Do
    begin
      Dec (a) ;
      { here i is the Huffman code of length k bits for value p^ }
      { make tables up to required level }
      while (k > w + l) do
      begin

        Inc (h) ;
        Inc (w, l);              { add bits already decoded }
                                 { previous table always l bits }
        { compute minimum size table less than or equal to l bits }

        { table size upper limit }
        z := g - w;
        If (z > uInt(l)) Then
          z := l;

        { try a k-w bit table }
        j := k - w;
        f := 1 shl j;
        if (f > a+1) Then        { too few codes for k-w bit table }
        begin
          Dec(f, a+1);           { deduct codes from patterns left }
          {$IFDEF USE_PTR}
          xp := Addr(c[k]);

          if (j < z) then
          begin
            Inc(j);
            while (j < z) do
            begin                { try smaller tables up to z bits }
              f := f shl 1;
              Inc (xp) ;
              If (f <= xp^) Then
                break;           { enough codes to use up j bits }
              Dec(f, xp^);       { else deduct codes from patterns }
              Inc(j);
            end;
          end;
          {$ELSE}
          xp := k;

          if (j < z) then
          begin
            Inc (j) ;
            While (j < z) Do
            begin                 { try smaller tables up to z bits }
              f := f * 2;
              Inc (xp) ;
              if (f <= c[xp]) then
                Break ;           { enough codes to use up j bits }
              Dec (f, c[xp]) ;      { else deduct codes from patterns }
              Inc (j);
            end;
          end;
          {$ENDIF}
        end;

        z := 1 shl j;            { table entries for j-bit table }

        { allocate new table }
        if (hn + z > MANY) then { (note: doesn't matter for fixed) }
        begin
          huft_build := Z_MEM_ERROR;     { not enough memory }
          exit;
        end;

        q := @hp[hn];
        u[h] := q;
        Inc(hn, z);

        { connect to last table, if there is one }
        if (h <> 0) then
        begin
          x[h] := i;             { save pattern for backing up }
          r.bits := Byte(l);     { bits to dump before this table }
          r.exop := Byte(j);     { bits in this table }
          j := i shr (w - l);
          {r.base := uInt( q - u[h-1] -j);}   { offset to this table }
          r.base := (ptr2int(q) - ptr2int(u[h-1]) ) div sizeof(q^) - j;
          huft_Ptr(u[h-1])^[j] := r;  { connect to last table }
        end
        else
          t^ := q;               { first table is returned result }
      end;

      { set up table entry in r }
      r.bits := Byte(k - w);

      { C-code: if (p >= v + n) - see ZUTIL.PAS for comments }

      if ptr2int(p)>=ptr2int(@(v[n])) then  { also works under DPMI ?? }
        r.exop := 128 + 64                  { out of values--invalid code }
      else
        if (p^ < s) then
        begin
          if (p^ < 256) then     { 256 is end-of-block code }
            r.exop := 0
          Else
            r.exop := 32 + 64;   { EOB_code; }
          r.base := p^;          { simple code is just the value }
          Inc(p);
        end
        Else
        begin
          r.exop := Byte(e[p^-s] + 16 + 64);  { non-simple--look up in lists }
          r.base := d[p^-s];
          Inc (p);
        end ;

      { fill code-like entries with r }
      f := 1 shl (k - w);
      j := i shr w;
      while (j < z) do
      begin
        huft_Ptr(q)^[j] := r;
        Inc(j, f);
      end;

      { backwards increment the k-bit code i }
      j := 1 shl (k-1) ;
      while (i and j) <> 0 do
      begin
        i := i xor j;         { bitwise exclusive or }
        j := j shr 1
      end ;
      i := i xor j;

      { backup over finished tables }
      mask := (1 shl w) - 1;   { needed on HP, cc -O bug }
      while ((i and mask) <> x[h]) do
      begin
        Dec(h);                { don't need to update q }
        Dec(w, l);
        mask := (1 shl w) - 1;
      end;

    end;

    Inc(k);
  end;

  { Return Z_BUF_ERROR if we were given an incomplete table }
  if (y <> 0) And (g <> 1) then
    huft_build := Z_BUF_ERROR
  else
    huft_build := Z_OK;
end; { huft_build}


function inflate_trees_bits(
  var c : array of uIntf;  { 19 code lengths }
  var bb : uIntf;          { bits tree desired/actual depth }
  var tb : pinflate_huft;  { bits tree result }
  var hp : array of Inflate_huft;      { space for trees }
  var z : z_stream         { for messages }
    ) : int;
var
  r : int;
  hn : uInt;          { hufts used in space }
  v : PuIntArray;     { work area for huft_build }
begin
  hn := 0;
  v := PuIntArray( ZALLOC(z, 19, sizeof(uInt)) );
  if (v = Z_NULL) then
  begin
    inflate_trees_bits := Z_MEM_ERROR;
    exit;
  end;

  r := huft_build(c, 19, 19, cplens, cplext,
                             {puIntf(Z_NULL), puIntf(Z_NULL),}
                  @tb, bb, hp, hn, v^);
  if (r = Z_DATA_ERROR) then
    z.msg := 'oversubscribed dynamic bit lengths tree'
  else
    if (r = Z_BUF_ERROR) or (bb = 0) then
    begin
      z.msg := 'incomplete dynamic bit lengths tree';
      r := Z_DATA_ERROR;
    end;
  ZFREE(z, v);
  inflate_trees_bits := r;
end;


function inflate_trees_dynamic(
    nl : uInt;                    { number of literal/length codes }
    nd : uInt;                    { number of distance codes }
    var c : Array of uIntf;           { that many (total) code lengths }
    var bl : uIntf;          { literal desired/actual bit depth }
    var bd : uIntf;          { distance desired/actual bit depth }
var tl : pInflate_huft;           { literal/length tree result }
var td : pInflate_huft;           { distance tree result }
var hp : array of Inflate_huft;   { space for trees }
var z : z_stream                  { for messages }
     ) : int;
var
  r : int;
  hn : uInt;          { hufts used in space }
  v : PuIntArray;     { work area for huft_build }
begin
  hn := 0;
  { allocate work area }
  v := PuIntArray( ZALLOC(z, 288, sizeof(uInt)) );
  if (v = Z_NULL) then
  begin
    inflate_trees_dynamic := Z_MEM_ERROR;
    exit;
  end;

  { build literal/length tree }
  r := huft_build(c, nl, 257, cplens, cplext, @tl, bl, hp, hn, v^);
  if (r <> Z_OK) or (bl = 0) then
  begin
    if (r = Z_DATA_ERROR) then
      z.msg := 'oversubscribed literal/length tree'
    else
      if (r <> Z_MEM_ERROR) then
      begin
        z.msg := 'incomplete literal/length tree';
        r := Z_DATA_ERROR;
      end;

    ZFREE(z, v);
    inflate_trees_dynamic := r;
    exit;
  end;

  { build distance tree }
  r := huft_build(puIntArray(@c[nl])^, nd, 0,
                  cpdist, cpdext, @td, bd, hp, hn, v^);
  if (r <> Z_OK) or ((bd = 0) and (nl > 257)) then
  begin
    if (r = Z_DATA_ERROR) then
      z.msg := 'oversubscribed literal/length tree'
    else
      if (r = Z_BUF_ERROR) then
      begin
{$ifdef PKZIP_BUG_WORKAROUND}
        r := Z_OK;
      end;
{$else}
        z.msg := 'incomplete literal/length tree';
        r := Z_DATA_ERROR;
      end
      else
        if (r <> Z_MEM_ERROR) then
        begin
          z.msg := 'empty distance tree with lengths';
          r := Z_DATA_ERROR;
        end;
    ZFREE(z, v);
    inflate_trees_dynamic := r;
    exit;
{$endif}
  end;

  { done }
  ZFREE(z, v);
  inflate_trees_dynamic := Z_OK;
end;

{$UNDEF BUILDFIXED}

{ build fixed tables only once--keep them here }
{$IFNDEF BUILDFIXED}
{ locals }
var
  fixed_built : Boolean = false;
const  
  FIXEDH = 544;      { number of hufts used by fixed tables }
var
  fixed_mem : array[0..FIXEDH-1] of inflate_huft;
  fixed_bl : uInt;
  fixed_bd : uInt;
  fixed_tl : pInflate_huft;
  fixed_td : pInflate_huft;

{$ELSE}

{ inffixed.h -- table for decoding fixed codes }

{local}
const
  fixed_bl = uInt(9);
{local}
const
  fixed_bd = uInt(5);
{local}
const
  fixed_tl : array [0..288-1] of inflate_huft = (
    Exop,             { number of extra bits or operation }
    bits : Byte;      { number of bits in this code or subcode }
    {pad : uInt;}       { pad structure to a power of 2 (4 bytes for }
                      {  16-bit, 8 bytes for 32-bit int's) }
    base : uInt;      { literal, length base, or distance base }
                      { or table offset }

    ((96,7),256), ((0,8),80), ((0,8),16), ((84,8),115), ((82,7),31),
    ((0,8),112), ((0,8),48), ((0,9),192), ((80,7),10), ((0,8),96),
    ((0,8),32), ((0,9),160), ((0,8),0), ((0,8),128), ((0,8),64),
    ((0,9),224), ((80,7),6), ((0,8),88), ((0,8),24), ((0,9),144),
    ((83,7),59), ((0,8),120), ((0,8),56), ((0,9),208), ((81,7),17),
    ((0,8),104), ((0,8),40), ((0,9),176), ((0,8),8), ((0,8),136),
    ((0,8),72), ((0,9),240), ((80,7),4), ((0,8),84), ((0,8),20),
    ((85,8),227), ((83,7),43), ((0,8),116), ((0,8),52), ((0,9),200),
    ((81,7),13), ((0,8),100), ((0,8),36), ((0,9),168), ((0,8),4),
    ((0,8),132), ((0,8),68), ((0,9),232), ((80,7),8), ((0,8),92),
    ((0,8),28), ((0,9),152), ((84,7),83), ((0,8),124), ((0,8),60),
    ((0,9),216), ((82,7),23), ((0,8),108), ((0,8),44), ((0,9),184),
    ((0,8),12), ((0,8),140), ((0,8),76), ((0,9),248), ((80,7),3),
    ((0,8),82), ((0,8),18), ((85,8),163), ((83,7),35), ((0,8),114),
    ((0,8),50), ((0,9),196), ((81,7),11), ((0,8),98), ((0,8),34),
    ((0,9),164), ((0,8),2), ((0,8),130), ((0,8),66), ((0,9),228),
    ((80,7),7), ((0,8),90), ((0,8),26), ((0,9),148), ((84,7),67),
    ((0,8),122), ((0,8),58), ((0,9),212), ((82,7),19), ((0,8),106),
    ((0,8),42), ((0,9),180), ((0,8),10), ((0,8),138), ((0,8),74),
    ((0,9),244), ((80,7),5), ((0,8),86), ((0,8),22), ((192,8),0),
    ((83,7),51), ((0,8),118), ((0,8),54), ((0,9),204), ((81,7),15),
    ((0,8),102), ((0,8),38), ((0,9),172), ((0,8),6), ((0,8),134),
    ((0,8),70), ((0,9),236), ((80,7),9), ((0,8),94), ((0,8),30),
    ((0,9),156), ((84,7),99), ((0,8),126), ((0,8),62), ((0,9),220),
    ((82,7),27), ((0,8),110), ((0,8),46), ((0,9),188), ((0,8),14),
    ((0,8),142), ((0,8),78), ((0,9),252), ((96,7),256), ((0,8),81),
    ((0,8),17), ((85,8),131), ((82,7),31), ((0,8),113), ((0,8),49),
    ((0,9),194), ((80,7),10), ((0,8),97), ((0,8),33), ((0,9),162),
    ((0,8),1), ((0,8),129), ((0,8),65), ((0,9),226), ((80,7),6),
    ((0,8),89), ((0,8),25), ((0,9),146), ((83,7),59), ((0,8),121),
    ((0,8),57), ((0,9),210), ((81,7),17), ((0,8),105), ((0,8),41),
    ((0,9),178), ((0,8),9), ((0,8),137), ((0,8),73), ((0,9),242),
    ((80,7),4), ((0,8),85), ((0,8),21), ((80,8),258), ((83,7),43),
    ((0,8),117), ((0,8),53), ((0,9),202), ((81,7),13), ((0,8),101),
    ((0,8),37), ((0,9),170), ((0,8),5), ((0,8),133), ((0,8),69),
    ((0,9),234), ((80,7),8), ((0,8),93), ((0,8),29), ((0,9),154),
    ((84,7),83), ((0,8),125), ((0,8),61), ((0,9),218), ((82,7),23),
    ((0,8),109), ((0,8),45), ((0,9),186), ((0,8),13), ((0,8),141),
    ((0,8),77), ((0,9),250), ((80,7),3), ((0,8),83), ((0,8),19),
    ((85,8),195), ((83,7),35), ((0,8),115), ((0,8),51), ((0,9),198),
    ((81,7),11), ((0,8),99), ((0,8),35), ((0,9),166), ((0,8),3),
    ((0,8),131), ((0,8),67), ((0,9),230), ((80,7),7), ((0,8),91),
    ((0,8),27), ((0,9),150), ((84,7),67), ((0,8),123), ((0,8),59),
    ((0,9),214), ((82,7),19), ((0,8),107), ((0,8),43), ((0,9),182),
    ((0,8),11), ((0,8),139), ((0,8),75), ((0,9),246), ((80,7),5),
    ((0,8),87), ((0,8),23), ((192,8),0), ((83,7),51), ((0,8),119),
    ((0,8),55), ((0,9),206), ((81,7),15), ((0,8),103), ((0,8),39),
    ((0,9),174), ((0,8),7), ((0,8),135), ((0,8),71), ((0,9),238),
    ((80,7),9), ((0,8),95), ((0,8),31), ((0,9),158), ((84,7),99),
    ((0,8),127), ((0,8),63), ((0,9),222), ((82,7),27), ((0,8),111),
    ((0,8),47), ((0,9),190), ((0,8),15), ((0,8),143), ((0,8),79),
    ((0,9),254), ((96,7),256), ((0,8),80), ((0,8),16), ((84,8),115),
    ((82,7),31), ((0,8),112), ((0,8),48), ((0,9),193), ((80,7),10),
    ((0,8),96), ((0,8),32), ((0,9),161), ((0,8),0), ((0,8),128),
    ((0,8),64), ((0,9),225), ((80,7),6), ((0,8),88), ((0,8),24),
    ((0,9),145), ((83,7),59), ((0,8),120), ((0,8),56), ((0,9),209),
    ((81,7),17), ((0,8),104), ((0,8),40), ((0,9),177), ((0,8),8),
    ((0,8),136), ((0,8),72), ((0,9),241), ((80,7),4), ((0,8),84),
    ((0,8),20), ((85,8),227), ((83,7),43), ((0,8),116), ((0,8),52),
    ((0,9),201), ((81,7),13), ((0,8),100), ((0,8),36), ((0,9),169),
    ((0,8),4), ((0,8),132), ((0,8),68), ((0,9),233), ((80,7),8),
    ((0,8),92), ((0,8),28), ((0,9),153), ((84,7),83), ((0,8),124),
    ((0,8),60), ((0,9),217), ((82,7),23), ((0,8),108), ((0,8),44),
    ((0,9),185), ((0,8),12), ((0,8),140), ((0,8),76), ((0,9),249),
    ((80,7),3), ((0,8),82), ((0,8),18), ((85,8),163), ((83,7),35),
    ((0,8),114), ((0,8),50), ((0,9),197), ((81,7),11), ((0,8),98),
    ((0,8),34), ((0,9),165), ((0,8),2), ((0,8),130), ((0,8),66),
    ((0,9),229), ((80,7),7), ((0,8),90), ((0,8),26), ((0,9),149),
    ((84,7),67), ((0,8),122), ((0,8),58), ((0,9),213), ((82,7),19),
    ((0,8),106), ((0,8),42), ((0,9),181), ((0,8),10), ((0,8),138),
    ((0,8),74), ((0,9),245), ((80,7),5), ((0,8),86), ((0,8),22),
    ((192,8),0), ((83,7),51), ((0,8),118), ((0,8),54), ((0,9),205),
    ((81,7),15), ((0,8),102), ((0,8),38), ((0,9),173), ((0,8),6),
    ((0,8),134), ((0,8),70), ((0,9),237), ((80,7),9), ((0,8),94),
    ((0,8),30), ((0,9),157), ((84,7),99), ((0,8),126), ((0,8),62),
    ((0,9),221), ((82,7),27), ((0,8),110), ((0,8),46), ((0,9),189),
    ((0,8),14), ((0,8),142), ((0,8),78), ((0,9),253), ((96,7),256),
    ((0,8),81), ((0,8),17), ((85,8),131), ((82,7),31), ((0,8),113),
    ((0,8),49), ((0,9),195), ((80,7),10), ((0,8),97), ((0,8),33),
    ((0,9),163), ((0,8),1), ((0,8),129), ((0,8),65), ((0,9),227),
    ((80,7),6), ((0,8),89), ((0,8),25), ((0,9),147), ((83,7),59),
    ((0,8),121), ((0,8),57), ((0,9),211), ((81,7),17), ((0,8),105),
    ((0,8),41), ((0,9),179), ((0,8),9), ((0,8),137), ((0,8),73),
    ((0,9),243), ((80,7),4), ((0,8),85), ((0,8),21), ((80,8),258),
    ((83,7),43), ((0,8),117), ((0,8),53), ((0,9),203), ((81,7),13),
    ((0,8),101), ((0,8),37), ((0,9),171), ((0,8),5), ((0,8),133),
    ((0,8),69), ((0,9),235), ((80,7),8), ((0,8),93), ((0,8),29),
    ((0,9),155), ((84,7),83), ((0,8),125), ((0,8),61), ((0,9),219),
    ((82,7),23), ((0,8),109), ((0,8),45), ((0,9),187), ((0,8),13),
    ((0,8),141), ((0,8),77), ((0,9),251), ((80,7),3), ((0,8),83),
    ((0,8),19), ((85,8),195), ((83,7),35), ((0,8),115), ((0,8),51),
    ((0,9),199), ((81,7),11), ((0,8),99), ((0,8),35), ((0,9),167),
    ((0,8),3), ((0,8),131), ((0,8),67), ((0,9),231), ((80,7),7),
    ((0,8),91), ((0,8),27), ((0,9),151), ((84,7),67), ((0,8),123),
    ((0,8),59), ((0,9),215), ((82,7),19), ((0,8),107), ((0,8),43),
    ((0,9),183), ((0,8),11), ((0,8),139), ((0,8),75), ((0,9),247),
    ((80,7),5), ((0,8),87), ((0,8),23), ((192,8),0), ((83,7),51),
    ((0,8),119), ((0,8),55), ((0,9),207), ((81,7),15), ((0,8),103),
    ((0,8),39), ((0,9),175), ((0,8),7), ((0,8),135), ((0,8),71),
    ((0,9),239), ((80,7),9), ((0,8),95), ((0,8),31), ((0,9),159),
    ((84,7),99), ((0,8),127), ((0,8),63), ((0,9),223), ((82,7),27),
    ((0,8),111), ((0,8),47), ((0,9),191), ((0,8),15), ((0,8),143),
    ((0,8),79), ((0,9),255)
  );

{local}
const
  fixed_td : array[0..32-1] of inflate_huft = (
(Exop:80;bits:5;base:1),      (Exop:87;bits:5;base:257),   (Exop:83;bits:5;base:17),
(Exop:91;bits:5;base:4097),   (Exop:81;bits:5;base),       (Exop:89;bits:5;base:1025),
(Exop:85;bits:5;base:65),     (Exop:93;bits:5;base:16385), (Exop:80;bits:5;base:3),
(Exop:88;bits:5;base:513),    (Exop:84;bits:5;base:33),    (Exop:92;bits:5;base:8193),
(Exop:82;bits:5;base:9),      (Exop:90;bits:5;base:2049),  (Exop:86;bits:5;base:129),
(Exop:192;bits:5;base:24577), (Exop:80;bits:5;base:2),     (Exop:87;bits:5;base:385),
(Exop:83;bits:5;base:25),     (Exop:91;bits:5;base:6145),  (Exop:81;bits:5;base:7),
(Exop:89;bits:5;base:1537),   (Exop:85;bits:5;base:97),    (Exop:93;bits:5;base:24577),
(Exop:80;bits:5;base:4),      (Exop:88;bits:5;base:769),   (Exop:84;bits:5;base:49),
(Exop:92;bits:5;base:12289),  (Exop:82;bits:5;base:13),    (Exop:90;bits:5;base:3073),
(Exop:86;bits:5;base:193),    (Exop:192;bits:5;base:24577)
  );
{$ENDIF}

function inflate_trees_fixed(
var bl : uInt;               { literal desired/actual bit depth }
var bd : uInt;               { distance desired/actual bit depth }
var tl : pInflate_huft;      { literal/length tree result }
var td : pInflate_huft;      { distance tree result }
var  z : z_stream            { for memory allocation }
      ) : int;
type
  pFixed_table = ^fixed_table;
  fixed_table = array[0..288-1] of uIntf;
var
  k : int;                   { temporary variable }
  c : pFixed_table;          { length list for huft_build }
  v : PuIntArray;            { work area for huft_build }
var
  f : uInt;                  { number of hufts used in fixed_mem }
begin
  { build fixed tables if not already (multiple overlapped executions ok) }
  if not fixed_built then
  begin
    f := 0;

    { allocate memory }
    c := pFixed_table( ZALLOC(z, 288, sizeof(uInt)) );
    if (c = Z_NULL) then
    begin
      inflate_trees_fixed := Z_MEM_ERROR;
      exit;
    end;
    v := PuIntArray( ZALLOC(z, 288, sizeof(uInt)) );
    if (v = Z_NULL) then
    begin
      ZFREE(z, c);
      inflate_trees_fixed := Z_MEM_ERROR;
      exit;
    end;

    { literal table }
    for k := 0 to Pred(144) do
      c^[k] := 8;
    for k := 144 to Pred(256) do
      c^[k] := 9;
    for k := 256 to Pred(280) do
      c^[k] := 7;
    for k := 280 to Pred(288) do
      c^[k] := 8;
    fixed_bl := 9;
    huft_build(c^, 288, 257, cplens, cplext, @fixed_tl, fixed_bl,
               fixed_mem, f, v^);

    { distance table }
    for k := 0 to Pred(30) do
      c^[k] := 5;
    fixed_bd := 5;
    huft_build(c^, 30, 0, cpdist, cpdext, @fixed_td, fixed_bd,
               fixed_mem, f, v^);

    { done }
    ZFREE(z, v);
    ZFREE(z, c);
    fixed_built := True;
  end;
  bl := fixed_bl;
  bd := fixed_bd;
  tl := fixed_tl;
  td := fixed_td;
  inflate_trees_fixed := Z_OK;
end; { inflate_trees_fixed }


// form inffast
{ Called with number of bytes left to write in window at least 258
  (the maximum string length) and number of input bytes available
  at least ten.  The ten bytes are six bytes for the longest length/
  distance pair plus four bytes for overloading the bit buffer. }

function inflate_fast( bl : uInt;
                       bd : uInt;
                       tl : pInflate_huft;
                       td : pInflate_huft;
                      var s : inflate_blocks_state;
                      var z : z_stream) : int;

var
  t : pInflate_huft;      { temporary pointer }
  e : uInt;               { extra bits or operation }
  b : uLong;              { bit buffer }
  k : uInt;               { bits in bit buffer }
  p : pBytef;             { input data pointer }
  n : uInt;               { bytes available there }
  q : pBytef;             { output window write pointer }
  m : uInt;               { bytes to end of window or read pointer }
  ml : uInt;              { mask for literal/length tree }
  md : uInt;              { mask for distance tree }
  c : uInt;               { bytes to copy }
  d : uInt;               { distance back to copy from }
  r : pBytef;             { copy source pointer }
begin
  { load input, output, bit values (macro LOAD) }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));

  { initialize masks }
  ml := inflate_mask[bl];
  md := inflate_mask[bd];

  { do until not enough input or output space for fast loop }
  repeat                      { assume called with (m >= 258) and (n >= 10) }
    { get literal/length code }
    {GRABBITS(20);}             { max bits for literal/length code }
    while (k < 20) do
    begin
      Dec(n);
      b := b or (uLong(p^) shl k);
      Inc(p);
      Inc(k, 8);
    end;

    t := @(huft_ptr(tl)^[uInt(b) and ml]);

    e := t^.exop;
    if (e = 0) then
    begin
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);
      q^ := Byte(t^.base);
      Inc(q);
      Dec(m);
      continue;
    end;
    repeat
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);

      if (e and 16 <> 0) then
      begin
        { get extra bits for length }
        e := e and 15;
        c := t^.base + (uInt(b) and inflate_mask[e]);
        {DUMPBITS(e);}
        b := b shr e;
        Dec(k, e);
        { decode distance base of block to copy }
        {GRABBITS(15);}           { max bits for distance code }
        while (k < 15) do
        begin
          Dec(n);
          b := b or (uLong(p^) shl k);
          Inc(p);
          Inc(k, 8);
        end;

        t := @huft_ptr(td)^[uInt(b) and md];
        e := t^.exop;
        repeat
          {DUMPBITS(t^.bits);}
          b := b shr t^.bits;
          Dec(k, t^.bits);

          if (e and 16 <> 0) then
          begin
            { get extra bits to add to distance base }
            e := e and 15;
            {GRABBITS(e);}         { get extra bits (up to 13) }
            while (k < e) do
            begin
              Dec(n);
              b := b or (uLong(p^) shl k);
              Inc(p);
              Inc(k, 8);
            end;

            d := t^.base + (uInt(b) and inflate_mask[e]);
            {DUMPBITS(e);}
            b := b shr e;
            Dec(k, e);

            { do the copy }
            Dec(m, c);
            if (uInt(ptr2int(q) - ptr2int(s.window)) >= d) then     { offset before dest }
            begin                                  {  just copy }
              r := q;
              Dec(r, d);
              q^ := r^;  Inc(q); Inc(r); Dec(c); { minimum count is three, }
              q^ := r^;  Inc(q); Inc(r); Dec(c); { so unroll loop a little }
            end
            else                        { else offset after destination }
            begin
              e := d - uInt(ptr2int(q) - ptr2int(s.window)); { bytes from offset to end }
              r := s.zend;
              Dec(r, e);                  { pointer to offset }
              if (c > e) then             { if source crosses, }
              begin
                Dec(c, e);                { copy to end of window }
                repeat
                  q^ := r^;
                  Inc(q);
                  Inc(r);
                  Dec(e);
                until (e=0);
                r := s.window;           { copy rest from start of window }
              end;
            end;
            repeat                       { copy all or what's left }
              q^ := r^;
              Inc(q);
              Inc(r);
              Dec(c);
            until (c = 0);
            break;
          end
          else
            if (e and 64 = 0) then
            begin
              Inc(t, t^.base + (uInt(b) and inflate_mask[e]));
              e := t^.exop;
            end
          else
          begin
            z.msg := 'invalid distance code';
            {UNGRAB}
            c := z.avail_in-n;
            if (k shr 3) < c then
              c := k shr 3;
            Inc(n, c);
            Dec(p, c);
            Dec(k, c shl 3);
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;

            inflate_fast := Z_DATA_ERROR;
            exit;
          end;
        until FALSE;
        break;
      end;
      if (e and 64 = 0) then
      begin
         {t += t->base;
          e = (t += ((uInt)b & inflate_mask[e]))->exop;}

        Inc(t, t^.base + (uInt(b) and inflate_mask[e]));
        e := t^.exop;
        if (e = 0) then
        begin
          {DUMPBITS(t^.bits);}
          b := b shr t^.bits;
          Dec(k, t^.bits);

          q^ := Byte(t^.base);
          Inc(q);
          Dec(m);
          break;
        end;
      end
      else
        if (e and 32 <> 0) then
        begin
          {UNGRAB}
          c := z.avail_in-n;
          if (k shr 3) < c then
            c := k shr 3;
          Inc(n, c);
          Dec(p, c);
          Dec(k, c shl 3);
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_fast := Z_STREAM_END;
          exit;
        end
        else
        begin
          z.msg := 'invalid literal/length code';
          {UNGRAB}
          c := z.avail_in-n;
          if (k shr 3) < c then
            c := k shr 3;
          Inc(n, c);
          Dec(p, c);
          Dec(k, c shl 3);
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_fast := Z_DATA_ERROR;
          exit;
        end;
    until FALSE;
  until (m < 258) or (n < 10);

  { not enough input or output--restore pointers and return }
  {UNGRAB}
  c := z.avail_in-n;
  if (k shr 3) < c then
    c := k shr 3;
  Inc(n, c);
  Dec(p, c);
  Dec(k, c shl 3);
  {UPDATE}
  s.bitb := b;
  s.bitk := k;
  z.avail_in := n;
  Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
  z.next_in := p;
  s.write := q;
  inflate_fast := Z_OK;
end;

// from infCodes
function inflate_codes_new (bl : uInt;
                            bd : uInt;
                            tl : pInflate_huft;
                            td : pInflate_huft;
                            var z : z_stream): pInflate_codes_state;
var
 c : pInflate_codes_state;
begin
  c := pInflate_codes_state( ZALLOC(z,1,sizeof(inflate_codes_state)) );
  if (c <> Z_NULL) then
  begin
    c^.mode := START;
    c^.lbits := Byte(bl);
    c^.dbits := Byte(bd);
    c^.ltree := tl;
    c^.dtree := td;
  end;
  inflate_codes_new := c;
end;


function inflate_codes(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;
var
  j : uInt;               { temporary storage }
  t : pInflate_huft;      { temporary pointer }
  e : uInt;               { extra bits or operation }
  b : uLong;              { bit buffer }
  k : uInt;               { bits in bit buffer }
  p : pBytef;             { input data pointer }
  n : uInt;               { bytes available there }
  q : pBytef;             { output window write pointer }
  m : uInt;               { bytes to end of window or read pointer }
  f : pBytef;             { pointer to copy strings from }
var
  c : pInflate_codes_state;
begin
  c := s.sub.decode.codes;  { codes state }

  { copy input/output information to locals }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));

  { process input and output based on current state }
  while True do
  case (c^.mode) of
    { waiting for "i:"=input, "o:"=output, "x:"=nothing }
  START:         { x: set up for LEN }
    begin
{$ifndef SLOW}
      if (m >= 258) and (n >= 10) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;

        r := inflate_fast(c^.lbits, c^.dbits, c^.ltree, c^.dtree, s, z);
        {LOAD}
        p := z.next_in;
        n := z.avail_in;
        b := s.bitb;
        k := s.bitk;
        q := s.write;
        if ptr2int(q) < ptr2int(s.read) then
          m := uInt(ptr2int(s.read)-ptr2int(q)-1)
        else
          m := uInt(ptr2int(s.zend)-ptr2int(q));

        if (r <> Z_OK) then
        begin
          if (r = Z_STREAM_END) then
            c^.mode := WASH
          else
            c^.mode := BADCODE;
          continue;    { break for switch-statement in C }
        end;
      end;
{$endif} { not SLOW }
      c^.sub.code.need := c^.lbits;
      c^.sub.code.tree := c^.ltree;
      c^.mode := LEN;  { falltrough }
    end;
  LEN:           { i: get length/literal/eob next }
    begin
      j := c^.sub.code.need;
      {NEEDBITS(j);}
      while (k < j) do
      begin
        {NEEDBYTE;}
        if (n <> 0) then
          r :=Z_OK
        else
        begin
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      t := c^.sub.code.tree;
      Inc(t, uInt(b) and inflate_mask[j]);
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);

      e := uInt(t^.exop);
      if (e = 0) then            { literal }
      begin
        c^.sub.lit := t^.base;
        c^.mode := LIT;
        continue;  { break switch statement }
      end;
      if (e and 16 <> 0) then            { length }
      begin
        c^.sub.copy.get := e and 15;
        c^.len := t^.base;
        c^.mode := LENEXT;
        continue;         { break C-switch statement }
      end;
      if (e and 64 = 0) then             { next table }
      begin
        c^.sub.code.need := e;
        c^.sub.code.tree := @huft_ptr(t)^[t^.base];
        continue;         { break C-switch statement }
      end;
      if (e and 32 <> 0) then            { end of block }
      begin
        c^.mode := WASH;
        continue;         { break C-switch statement }
      end;
      c^.mode := BADCODE;        { invalid code }
      z.msg := 'invalid literal/length code';
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  LENEXT:        { i: getting length extra (have base) }
    begin
      j := c^.sub.copy.get;
      {NEEDBITS(j);}
      while (k < j) do
      begin
        {NEEDBYTE;}
        if (n <> 0) then
          r :=Z_OK
        else
        begin
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      Inc(c^.len, uInt(b and inflate_mask[j]));
      {DUMPBITS(j);}
      b := b shr j;
      Dec(k, j);

      c^.sub.code.need := c^.dbits;
      c^.sub.code.tree := c^.dtree;
      c^.mode := DIST;
      { falltrough }
    end;
  DIST:          { i: get distance next }
    begin
      j := c^.sub.code.need;
      {NEEDBITS(j);}
      while (k < j) do
      begin
        {NEEDBYTE;}
        if (n <> 0) then
          r :=Z_OK
        else
        begin
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      t := @huft_ptr(c^.sub.code.tree)^[uInt(b) and inflate_mask[j]];
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);

      e := uInt(t^.exop);
      if (e and 16 <> 0) then            { distance }
      begin
        c^.sub.copy.get := e and 15;
        c^.sub.copy.dist := t^.base;
        c^.mode := DISTEXT;
        continue;     { break C-switch statement }
      end;
      if (e and 64 = 0) then     { next table }
      begin
        c^.sub.code.need := e;
        c^.sub.code.tree := @huft_ptr(t)^[t^.base];
        continue;     { break C-switch statement }
      end;
      c^.mode := BADCODE;        { invalid code }
      z.msg := 'invalid distance code';
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  DISTEXT:       { i: getting distance extra }
    begin
      j := c^.sub.copy.get;
      {NEEDBITS(j);}
      while (k < j) do
      begin
        {NEEDBYTE;}
        if (n <> 0) then
          r :=Z_OK
        else
        begin
          {UPDATE}
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      Inc(c^.sub.copy.dist, uInt(b) and inflate_mask[j]);
      {DUMPBITS(j);}
      b := b shr j;
      Dec(k, j);
      c^.mode := COPY;
      { falltrough }
    end;
  COPY:          { o: copying bytes in window, waiting for space }
    begin
      f := q;
      Dec(f, c^.sub.copy.dist);
      if (uInt(ptr2int(q) - ptr2int(s.window)) < c^.sub.copy.dist) then
      begin
        f := s.zend;
        Dec(f, c^.sub.copy.dist - uInt(ptr2int(q) - ptr2int(s.window)));
      end;

      while (c^.len <> 0) do
      begin
        {NEEDOUT}
        if (m = 0) then
        begin
          {WRAP}
          if (q = s.zend) and (s.read <> s.window) then
          begin
            q := s.window;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));
          end;

          if (m = 0) then
          begin
            {FLUSH}
            s.write := q;
            r := inflate_flush(s,z,r);
            q := s.write;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));

            {WRAP}
            if (q = s.zend) and (s.read <> s.window) then
            begin
              q := s.window;
              if ptr2int(q) < ptr2int(s.read) then
                m := uInt(ptr2int(s.read)-ptr2int(q)-1)
              else
                m := uInt(ptr2int(s.zend)-ptr2int(q));
            end;

            if (m = 0) then
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_codes := inflate_flush(s,z,r);
              exit;
            end;
          end;
        end;
        r := Z_OK;

        {OUTBYTE( *f++)}
        q^ := f^;
        Inc(q);
        Inc(f);
        Dec(m);

        if (f = s.zend) then
          f := s.window;
        Dec(c^.len);
      end;
      c^.mode := START;
      { C-switch break; not needed }
    end;
  LIT:           { o: got literal, waiting for output space }
    begin
      {NEEDOUT}
      if (m = 0) then
      begin
        {WRAP}
        if (q = s.zend) and (s.read <> s.window) then
        begin
          q := s.window;
          if ptr2int(q) < ptr2int(s.read) then
            m := uInt(ptr2int(s.read)-ptr2int(q)-1)
          else
            m := uInt(ptr2int(s.zend)-ptr2int(q));
        end;

        if (m = 0) then
        begin
          {FLUSH}
          s.write := q;
          r := inflate_flush(s,z,r);
          q := s.write;
          if ptr2int(q) < ptr2int(s.read) then
            m := uInt(ptr2int(s.read)-ptr2int(q)-1)
          else
            m := uInt(ptr2int(s.zend)-ptr2int(q));

          {WRAP}
          if (q = s.zend) and (s.read <> s.window) then
          begin
            q := s.window;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));
          end;

          if (m = 0) then
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_codes := inflate_flush(s,z,r);
            exit;
          end;
        end;
      end;
      r := Z_OK;

      {OUTBYTE(c^.sub.lit);}
      q^ := c^.sub.lit;
      Inc(q);
      Dec(m);

      c^.mode := START;
      {break;}
    end;
  WASH:          { o: got eob, possibly more output }
    begin
      {$ifdef patch112}
      if (k > 7) then           { return unused byte, if any }
      begin
        Dec(k, 8);
        Inc(n);
        Dec(p);                    { can always return one }
      end;
      {$endif}
      {FLUSH}
      s.write := q;
      r := inflate_flush(s,z,r);
      q := s.write;
      if ptr2int(q) < ptr2int(s.read) then
        m := uInt(ptr2int(s.read)-ptr2int(q)-1)
      else
        m := uInt(ptr2int(s.zend)-ptr2int(q));

      if (s.read <> s.write) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;
        inflate_codes := inflate_flush(s,z,r);
        exit;
      end;
      c^.mode := ZEND;
      { falltrough }
    end;

  ZEND:
    begin
      r := Z_STREAM_END;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  BADCODE:       { x: got error }
    begin
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  else
    begin
      r := Z_STREAM_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  end;
{NEED_DUMMY_RETURN - Delphi2+ dumb compilers complain without this }
  inflate_codes := Z_STREAM_ERROR;
end;


procedure inflate_codes_free(c : pInflate_codes_state;
                             var z : z_stream);
begin
  ZFREE(z, c);
end;

// from infblock
{ Tables for deflate from PKZIP's appnote.txt. }
Const
  border : Array [0..18] Of Word  { Order of the bit length code lengths }
    = (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

{ Notes beyond the 1.93a appnote.txt:

   1. Distance pointers never point before the beginning of the output
      stream.
   2. Distance pointers can point back across blocks, up to 32k away.
   3. There is an implied maximum of 7 bits for the bit length table and
      15 bits for the actual data.
   4. If only one code exists, then it is encoded using one bit.  (Zero
      would be more efficient, but perhaps a little confusing.)  If two
      codes exist, they are coded using one bit each (0 and 1).
   5. There is no way of sending zero distance codes--a dummy must be
      sent if there are none.  (History: a pre 2.0 version of PKZIP would
      store blocks with no distance codes, but this was discovered to be
      too harsh a criterion.)  Valid only for 1.93a.  2.04c does allow
      zero distance codes, which is sent as one code of zero bits in
      length.
   6. There are up to 286 literal/length codes.  Code 256 represents the
      end-of-block.  Note however that the static length tree defines
      288 codes just to fill out the Huffman codes.  Codes 286 and 287
      cannot be used though, since there is no length base or extra bits
      defined for them.  Similarily, there are up to 30 distance codes.
      However, static trees define 32 codes (all 5 bits) to fill out the
      Huffman codes, but the last two had better not show up in the data.
   7. Unzip can check dynamic Huffman blocks for complete code sets.
      The exception is that a single code would not be complete (see #4).
   8. The five bits following the block type is really the number of
      literal codes sent minus 257.
   9. Length codes 8,16,16 are interpreted as 13 length codes of 8 bits
      (1+6+6).  Therefore, to output three times the length, you output
      three codes (1+1+1), whereas to output four times the same length,
      you only need two codes (1+3).  Hmm.
  10. In the tree reconstruction algorithm, Code = Code + Increment
      only if BitLength(i) is not zero.  (Pretty obvious.)
  11. Correction: 4 Bits: # of Bit Length codes - 4     (4 - 19)
  12. Note: length code 284 can represent 227-258, but length code 285
      really is 258.  The last length deserves its own, short code
      since it gets used a lot in very redundant files.  The length
      258 is special since 258 - 3 (the min match length) is 255.
  13. The literal/length and distance code bit lengths are read as a
      single stream of lengths.  It is possible (and advantageous) for
      a repeat code (16, 17, or 18) to go across the boundary between
      the two sets of lengths. }


procedure inflate_blocks_reset (var s : inflate_blocks_state;
                                var z : z_stream;
                                c : puLong); { check value on output }
begin
  if (c <> Z_NULL) then
    c^ := s.check;
  if (s.mode = BTREE) or (s.mode = DTREE) then
    ZFREE(z, s.sub.trees.blens);
  if (s.mode = CODES) then
    inflate_codes_free(s.sub.decode.codes, z);

  s.mode := ZTYPE;
  s.bitk := 0;
  s.bitb := 0;

  s.write := s.window;
  s.read := s.window;
  if Assigned(s.checkfn) then
  begin
    s.check := s.checkfn(uLong(0), pBytef(NIL), 0);
    z.adler := s.check;
  end;
end;


function inflate_blocks_new(var z : z_stream;
                            c : check_func;  { check function }
                            w : uInt         { window size }
                            ) : pInflate_blocks_state;
var
  s : pInflate_blocks_state;
begin
  s := pInflate_blocks_state( ZALLOC(z,1, sizeof(inflate_blocks_state)) );
  if (s = Z_NULL) then
  begin
    inflate_blocks_new := s;
    exit;
  end;
  s^.hufts := huft_ptr( ZALLOC(z, sizeof(inflate_huft), MANY) );

  if (s^.hufts = Z_NULL) then
  begin
    ZFREE(z, s);
    inflate_blocks_new := Z_NULL;
    exit;
  end;

  s^.window := pBytef( ZALLOC(z, 1, w) );
  if (s^.window = Z_NULL) then
  begin
    ZFREE(z, s^.hufts);
    ZFREE(z, s);
    inflate_blocks_new := Z_NULL;
    exit;
  end;
  s^.zend := s^.window;
  Inc(s^.zend, w);
  s^.checkfn := c;
  s^.mode := ZTYPE;
  inflate_blocks_reset(s^, z, Z_NULL);
  inflate_blocks_new := s;
end;


function inflate_blocks (var s : inflate_blocks_state;
                         var z : z_stream;
                         r : int) : int;           { initial return code }
label
  start_btree, start_dtree,
  start_blkdone, start_dry,
  start_codes;

var
  t : uInt;               { temporary storage }
  b : uLong;              { bit buffer }
  k : uInt;               { bits in bit buffer }
  p : pBytef;             { input data pointer }
  n : uInt;               { bytes available there }
  q : pBytef;             { output window write pointer }
  m : uInt;               { bytes to end of window or read pointer }
{ fixed code blocks }
var
  bl, bd : uInt;
  tl, td : pInflate_huft;
var
  h : pInflate_huft;
  i, j, c : uInt;
var
  cs : pInflate_codes_state;
begin
  { copy input/output information to locals }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));

{ decompress an inflated block }


  { process input based on current state }
  while True do
  Case s.mode of
    ZTYPE:
      begin
        {NEEDBITS(3);}
        while (k < 3) do
        begin
          {NEEDBYTE;}
          if (n <> 0) then
            r :=Z_OK
          else
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          Dec(n);
          b := b or (uLong(p^) shl k);
          Inc(p);
          Inc(k, 8);
        end;

        t := uInt(b) and 7;
        s.last := boolean(t and 1);
        case (t shr 1) of
          0:                         { stored }
            begin
              {DUMPBITS(3);}
              b := b shr 3;
              Dec(k, 3);

              t := k and 7;                  { go to byte boundary }
              {DUMPBITS(t);}
              b := b shr t;
              Dec(k, t);

              s.mode := LENS;                { get length of stored block }
            end;
          1:                         { fixed }
            begin
              begin
                inflate_trees_fixed(bl, bd, tl, td, z);
                s.sub.decode.codes := inflate_codes_new(bl, bd, tl, td, z);
                if (s.sub.decode.codes = Z_NULL) then
                begin
                  r := Z_MEM_ERROR;
                  { update pointers and return }
                  s.bitb := b;
                  s.bitk := k;
                  z.avail_in := n;
                  Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                  z.next_in := p;
                  s.write := q;
                  inflate_blocks := inflate_flush(s,z,r);
                  exit;
                end;
              end;
              {DUMPBITS(3);}
              b := b shr 3;
              Dec(k, 3);

              s.mode := CODES;
            end;
          2:                         { dynamic }
            begin
              {DUMPBITS(3);}
              b := b shr 3;
              Dec(k, 3);

              s.mode := TABLE;
            end;
          3:
            begin                   { illegal }
              {DUMPBITS(3);}
              b := b shr 3;
              Dec(k, 3);

              s.mode := BLKBAD;
              z.msg := 'invalid block type';
              r := Z_DATA_ERROR;
              { update pointers and return }
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
        end;
      end;
    LENS:
      begin
        {NEEDBITS(32);}
        while (k < 32) do
        begin
          {NEEDBYTE;}
          if (n <> 0) then
            r :=Z_OK
          else
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          Dec(n);
          b := b or (uLong(p^) shl k);
          Inc(p);
          Inc(k, 8);
        end;

        if (((not b) shr 16) and $ffff) <> (b and $ffff) then
        begin
          s.mode := BLKBAD;
          z.msg := 'invalid stored block lengths';
          r := Z_DATA_ERROR;
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        s.sub.left := uInt(b) and $ffff;
        k := 0;
        b := 0;                      { dump bits }
        if s.sub.left <> 0 then
          s.mode := STORED
        else
          if s.last then
            s.mode := DRY
          else
            s.mode := ZTYPE;
      end;
    STORED:
      begin
        if (n = 0) then
        begin
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        {NEEDOUT}
        if (m = 0) then
        begin
          {WRAP}
          if (q = s.zend) and (s.read <> s.window) then
          begin
            q := s.window;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));
          end;

          if (m = 0) then
          begin
            {FLUSH}
            s.write := q;
            r := inflate_flush(s,z,r);
            q := s.write;
            if ptr2int(q) < ptr2int(s.read) then
              m := uInt(ptr2int(s.read)-ptr2int(q)-1)
            else
              m := uInt(ptr2int(s.zend)-ptr2int(q));

            {WRAP}
            if (q = s.zend) and (s.read <> s.window) then
            begin
              q := s.window;
              if ptr2int(q) < ptr2int(s.read) then
                m := uInt(ptr2int(s.read)-ptr2int(q)-1)
              else
                m := uInt(ptr2int(s.zend)-ptr2int(q));
            end;

            if (m = 0) then
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
          end;
        end;
        r := Z_OK;

        t := s.sub.left;
        if (t > n) then
          t := n;
        if (t > m) then
          t := m;
        zmemcpy(q, p, t);
        Inc(p, t);  Dec(n, t);
        Inc(q, t);  Dec(m, t);
        Dec(s.sub.left, t);
        if (s.sub.left = 0) then
        begin
          if s.last then
            s.mode := DRY
          else
            s.mode := ZTYPE;
        end;
      end;
    TABLE:
      begin
        {NEEDBITS(14);}
        while (k < 14) do
        begin
          {NEEDBYTE;}
          if (n <> 0) then
            r :=Z_OK
          else
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          Dec(n);
          b := b or (uLong(p^) shl k);
          Inc(p);
          Inc(k, 8);
        end;

        t := uInt(b) and $3fff;
        s.sub.trees.table := t;
  {$ifndef PKZIP_BUG_WORKAROUND}
        if ((t and $1f) > 29) or (((t shr 5) and $1f) > 29) then
        begin
          s.mode := BLKBAD;
          z.msg := 'too many length or distance symbols';
          r := Z_DATA_ERROR;
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
  {$endif}
        t := 258 + (t and $1f) + ((t shr 5) and $1f);
        s.sub.trees.blens := puIntArray( ZALLOC(z, t, sizeof(uInt)) );
        if (s.sub.trees.blens = Z_NULL) then
        begin
          r := Z_MEM_ERROR;
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        {DUMPBITS(14);}
        b := b shr 14;
        Dec(k, 14);

        s.sub.trees.index := 0;
        s.mode := BTREE;
        { fall trough case is handled by the while }
        { try GOTO for speed - Nomssi }
        goto start_btree;
      end;
    BTREE:
      begin
        start_btree:
        while (s.sub.trees.index < 4 + (s.sub.trees.table shr 10)) do
        begin
          {NEEDBITS(3);}
          while (k < 3) do
          begin
            {NEEDBYTE;}
            if (n <> 0) then
              r :=Z_OK
            else
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
            Dec(n);
            b := b or (uLong(p^) shl k);
            Inc(p);
            Inc(k, 8);
          end;

          s.sub.trees.blens^[border[s.sub.trees.index]] := uInt(b) and 7;
          Inc(s.sub.trees.index);
          {DUMPBITS(3);}
          b := b shr 3;
          Dec(k, 3);
        end;
        while (s.sub.trees.index < 19) do
        begin
          s.sub.trees.blens^[border[s.sub.trees.index]] := 0;
          Inc(s.sub.trees.index);
        end;
        s.sub.trees.bb := 7;
        t := inflate_trees_bits(s.sub.trees.blens^, s.sub.trees.bb,
                                s.sub.trees.tb, s.hufts^, z);
        if (t <> Z_OK) then
        begin
          ZFREE(z, s.sub.trees.blens);
          r := t;
          if (r = Z_DATA_ERROR) then
            s.mode := BLKBAD;
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        s.sub.trees.index := 0;
        s.mode := DTREE;
        { fall through again }
        goto start_dtree;
      end;
    DTREE:
      begin
        start_dtree:
        while TRUE do
        begin
          t := s.sub.trees.table;
          if not (s.sub.trees.index < 258 +
                                     (t and $1f) + ((t shr 5) and $1f)) then
            break;
          t := s.sub.trees.bb;
          {NEEDBITS(t);}
          while (k < t) do
          begin
            {NEEDBYTE;}
            if (n <> 0) then
              r :=Z_OK
            else
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
            Dec(n);
            b := b or (uLong(p^) shl k);
            Inc(p);
            Inc(k, 8);
          end;

          h := s.sub.trees.tb;
          Inc(h, uInt(b) and inflate_mask[t]);
          t := h^.Bits;
          c := h^.Base;

          if (c < 16) then
          begin
            {DUMPBITS(t);}
            b := b shr t;
            Dec(k, t);

            s.sub.trees.blens^[s.sub.trees.index] := c;
            Inc(s.sub.trees.index);
          end
          else { c = 16..18 }
          begin
            if c = 18 then
            begin
              i := 7;
              j := 11;
            end
            else
            begin
              i := c - 14;
              j := 3;
            end;
            {NEEDBITS(t + i);}
            while (k < t + i) do
            begin
              {NEEDBYTE;}
              if (n <> 0) then
                r :=Z_OK
              else
              begin
                {UPDATE}
                s.bitb := b;
                s.bitk := k;
                z.avail_in := n;
                Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
                z.next_in := p;
                s.write := q;
                inflate_blocks := inflate_flush(s,z,r);
                exit;
              end;
              Dec(n);
              b := b or (uLong(p^) shl k);
              Inc(p);
              Inc(k, 8);
            end;

            {DUMPBITS(t);}
            b := b shr t;
            Dec(k, t);

            Inc(j, uInt(b) and inflate_mask[i]);
            {DUMPBITS(i);}
            b := b shr i;
            Dec(k, i);

            i := s.sub.trees.index;
            t := s.sub.trees.table;
            if (i + j > 258 + (t and $1f) + ((t shr 5) and $1f)) or
               ((c = 16) and (i < 1)) then
            begin
              ZFREE(z, s.sub.trees.blens);
              s.mode := BLKBAD;
              z.msg := 'invalid bit length repeat';
              r := Z_DATA_ERROR;
              { update pointers and return }
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              inflate_blocks := inflate_flush(s,z,r);
              exit;
            end;
            if c = 16 then
              c := s.sub.trees.blens^[i - 1]
            else
              c := 0;
            repeat
              s.sub.trees.blens^[i] := c;
              Inc(i);
              Dec(j);
            until (j=0);
            s.sub.trees.index := i;
          end;
        end; { while }
        s.sub.trees.tb := Z_NULL;
        begin
          bl := 9;         { must be <= 9 for lookahead assumptions }
          bd := 6;         { must be <= 9 for lookahead assumptions }
          t := s.sub.trees.table;
          t := inflate_trees_dynamic(257 + (t and $1f),
                  1 + ((t shr 5) and $1f),
                  s.sub.trees.blens^, bl, bd, tl, td, s.hufts^, z);
          ZFREE(z, s.sub.trees.blens);
          if (t <> Z_OK) then
          begin
            if (t = uInt(Z_DATA_ERROR)) then
              s.mode := BLKBAD;
            r := t;
            { update pointers and return }
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          { c renamed to cs }
          cs := inflate_codes_new(bl, bd, tl, td, z);
          if (cs = Z_NULL) then
          begin
            r := Z_MEM_ERROR;
            { update pointers and return }
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            inflate_blocks := inflate_flush(s,z,r);
            exit;
          end;
          s.sub.decode.codes := cs;
        end;
        s.mode := CODES;
        { yet another falltrough }
        goto start_codes;
      end;
    CODES:
      begin
        start_codes:
        { update pointers }
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;

        r := inflate_codes(s, z, r);
        if (r <> Z_STREAM_END) then
        begin
          inflate_blocks := inflate_flush(s, z, r);
          exit;
        end;
        r := Z_OK;
        inflate_codes_free(s.sub.decode.codes, z);
        { load local pointers }
        p := z.next_in;
        n := z.avail_in;
        b := s.bitb;
        k := s.bitk;
        q := s.write;
        if ptr2int(q) < ptr2int(s.read) then
          m := uInt(ptr2int(s.read)-ptr2int(q)-1)
        else
          m := uInt(ptr2int(s.zend)-ptr2int(q));
        if (not s.last) then
        begin
          s.mode := ZTYPE;
          continue; { break for switch statement in C-code }
        end;
        {$ifndef patch112}
        if (k > 7) then           { return unused byte, if any }
        begin
          Dec(k, 8);
          Inc(n);
          Dec(p);                    { can always return one }
        end;
        {$endif}
        s.mode := DRY;
        { another falltrough }
        goto start_dry;
      end;
    DRY:
      begin
        start_dry:
        {FLUSH}
        s.write := q;
        r := inflate_flush(s,z,r);
        q := s.write;

        { not needed anymore, we are done:
        if ptr2int(q) < ptr2int(s.read) then
          m := uInt(ptr2int(s.read)-ptr2int(q)-1)
        else
          m := uInt(ptr2int(s.zend)-ptr2int(q));
        }

        if (s.read <> s.write) then
        begin
          { update pointers and return }
          s.bitb := b;
          s.bitk := k;
          z.avail_in := n;
          Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
          z.next_in := p;
          s.write := q;
          inflate_blocks := inflate_flush(s,z,r);
          exit;
        end;
        s.mode := BLKDONE;
        goto start_blkdone;
      end;
    BLKDONE:
      begin
        start_blkdone:
        r := Z_STREAM_END;
        { update pointers and return }
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;
        inflate_blocks := inflate_flush(s,z,r);
        exit;
      end;
    BLKBAD:
      begin
        r := Z_DATA_ERROR;
        { update pointers and return }
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;
        inflate_blocks := inflate_flush(s,z,r);
        exit;
      end;
    else
    begin
      r := Z_STREAM_ERROR;
      { update pointers and return }
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_blocks := inflate_flush(s,z,r);
      exit;
    end;
  end; { Case s.mode of }

end;


function inflate_blocks_free(s : pInflate_blocks_state;
                             var z : z_stream) : int;
begin
  inflate_blocks_reset(s^, z, Z_NULL);
  ZFREE(z, s^.window);
  ZFREE(z, s^.hufts);
  ZFREE(z, s);
  inflate_blocks_free := Z_OK;
end;


procedure inflate_set_dictionary(var s : inflate_blocks_state;
                                 const d : array of byte; { dictionary }
                                 n : uInt);         { dictionary length }
begin
  zmemcpy(s.window, pBytef(@d), n);
  s.write := s.window;
  Inc(s.write, n);
  s.read := s.write;
end;


{ Returns true if inflate is currently at the end of a block generated
  by Z_SYNC_FLUSH or Z_FULL_FLUSH.
  IN assertion: s <> Z_NULL }

function inflate_blocks_sync_point(var s : inflate_blocks_state) : int;
begin
  inflate_blocks_sync_point := int(s.mode = LENS);
end;

// from zinflate
function inflateReset(var z : z_stream) : int;
begin
  if (z.state = Z_NULL) then
  begin
    inflateReset :=  Z_STREAM_ERROR;
    exit;
  end;
  z.total_out := 0;
  z.total_in := 0;
  z.msg := '';
  if z.state^.nowrap then
    z.state^.mode := BLOCKS
  else
    z.state^.mode := METHOD;
  inflate_blocks_reset(z.state^.blocks^, z, Z_NULL);
  inflateReset :=  Z_OK;
end;


function inflateEnd(var z : z_stream) : int;
begin
  if (z.state = Z_NULL) or not Assigned(z.zfree) then
  begin
    inflateEnd :=  Z_STREAM_ERROR;
    exit;
  end;
  if (z.state^.blocks <> Z_NULL) then
    inflate_blocks_free(z.state^.blocks, z);
  ZFREE(z, z.state);
  z.state := Z_NULL;
  inflateEnd :=  Z_OK;
end;


function inflateInit2_(var z: z_stream;
                       w : int;
                       const version : string;
                       stream_size : int) : int;
begin
  if (version = '') or (version[1] <> ZLIB_VERSION[1]) or
      (stream_size <> sizeof(z_stream)) then
  begin
    inflateInit2_ := Z_VERSION_ERROR;
    exit;
  end;
  { initialize state }
  { SetLength(strm.msg, 255); }
  z.msg := '';
  if not Assigned(z.zalloc) then
  begin
    {$IFDEF FPC}  z.zalloc := @zcalloc;  {$ELSE}
    z.zalloc := zcalloc;
    {$endif}
    z.opaque := voidpf(0);
  end;
  if not Assigned(z.zfree) then
    {$IFDEF FPC}  z.zfree := @zcfree;  {$ELSE}
    z.zfree := zcfree;
    {$ENDIF}

  z.state := pInternal_state( ZALLOC(z,1,sizeof(internal_state)) );
  if (z.state = Z_NULL) then
  begin
    inflateInit2_ := Z_MEM_ERROR;
    exit;
  end;

  z.state^.blocks := Z_NULL;

  { handle undocumented nowrap option (no zlib header or check) }
  z.state^.nowrap := FALSE;
  if (w < 0) then
  begin
    w := - w;
    z.state^.nowrap := TRUE;
  end;

  { set window size }
  if (w < 8) or (w > 15) then
  begin
    inflateEnd(z);
    inflateInit2_ := Z_STREAM_ERROR;
    exit;
  end;
  z.state^.wbits := uInt(w);

  { create inflate_blocks state }
  if z.state^.nowrap then
    z.state^.blocks := inflate_blocks_new(z, NIL, uInt(1) shl w)
  else
  {$IFDEF FPC}
    z.state^.blocks := inflate_blocks_new(z, @adler32, uInt(1) shl w);
  {$ELSE}
    z.state^.blocks := inflate_blocks_new(z, adler32, uInt(1) shl w);
  {$ENDIF}
  if (z.state^.blocks = Z_NULL) then
  begin
    inflateEnd(z);
    inflateInit2_ := Z_MEM_ERROR;
    exit;
  end;
  { reset state }
  inflateReset(z);
  inflateInit2_ :=  Z_OK;
end;

function inflateInit2(var z: z_stream; windowBits : int) : int;
begin
  inflateInit2 := inflateInit2_(z, windowBits, ZLIB_VERSION, sizeof(z_stream));
end;


function inflateInit(var z : z_stream) : int;
{ inflateInit is a macro to allow checking the zlib version
  and the compiler's view of z_stream:  }
begin
  inflateInit := inflateInit2_(z, DEF_WBITS, ZLIB_VERSION, sizeof(z_stream));
end;

function inflateInit_(z : z_streamp;
                      const version : string;
                      stream_size : int) : int;
begin
  { initialize state }
  if (z = Z_NULL) then
    inflateInit_ := Z_STREAM_ERROR
  else
    inflateInit_ := inflateInit2_(z^, DEF_WBITS, version, stream_size);
end;

function inflate(var z : z_stream;
                 f : int) : int;
var
  r : int;
  b : uInt;
begin
  if (z.state = Z_NULL) or (z.next_in = Z_NULL) then
  begin
    inflate := Z_STREAM_ERROR;
    exit;
  end;
  if f = Z_FINISH then
    f := Z_BUF_ERROR
  else
    f := Z_OK;
  r := Z_BUF_ERROR;
  while True do
  case (z.state^.mode) of
    BLOCKS:
      begin
        r := inflate_blocks(z.state^.blocks^, z, r);
        if (r = Z_DATA_ERROR) then
        begin
          z.state^.mode := BAD;
          z.state^.sub.marker := 0;       { can try inflateSync }
          continue;            { break C-switch }
        end;
        if (r = Z_OK) then
          r := f;
        if (r <> Z_STREAM_END) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        inflate_blocks_reset(z.state^.blocks^, z, @z.state^.sub.check.was);
        if (z.state^.nowrap) then
        begin
          z.state^.mode := DONE;
          continue;            { break C-switch }
        end;
        z.state^.mode := CHECK4;  { falltrough }
      end;
    CHECK4:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {z.state^.sub.check.need := uLong(NEXTBYTE(z)) shl 24;}
        Dec(z.avail_in);
        Inc(z.total_in);
        z.state^.sub.check.need := uLong(z.next_in^) shl 24;
        Inc(z.next_in);

        z.state^.mode := CHECK3;   { falltrough }
      end;
    CHECK3:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        {Inc( z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 16);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 16);
        Inc(z.next_in);

        z.state^.mode := CHECK2;   { falltrough }
      end;
    CHECK2:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {Inc( z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 8);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 8);
        Inc(z.next_in);

        z.state^.mode := CHECK1;   { falltrough }
      end;
    CHECK1:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        {Inc( z.state^.sub.check.need, uLong(NEXTBYTE(z)) );}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) );
        Inc(z.next_in);


        if (z.state^.sub.check.was <> z.state^.sub.check.need) then
        begin
          z.state^.mode := BAD;
          z.msg := 'incorrect data check';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue;           { break C-switch }
        end;
        z.state^.mode := DONE; { falltrough }
      end;
    DONE:
      begin
        inflate := Z_STREAM_END;
        exit;
      end;
    METHOD:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f; {}

        {z.state^.sub.method := NEXTBYTE(z);}
        Dec(z.avail_in);
        Inc(z.total_in);
        z.state^.sub.method := z.next_in^;
        Inc(z.next_in);

        if ((z.state^.sub.method and $0f) <> Z_DEFLATED) then
        begin
          z.state^.mode := BAD;
          z.msg := 'unknown compression method';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue;  { break C-switch }
        end;
        if ((z.state^.sub.method shr 4) + 8 > z.state^.wbits) then
        begin
          z.state^.mode := BAD;
          z.msg := 'invalid window size';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue; { break C-switch }
        end;
        z.state^.mode := FLAG;
        { fall trough }
      end;
    FLAG:
      begin
        {NEEDBYTE}
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f; {}
        {b := NEXTBYTE(z);}
        Dec(z.avail_in);
        Inc(z.total_in);
        b := z.next_in^;
        Inc(z.next_in);

        if (((z.state^.sub.method shl 8) + b) mod 31) <> 0 then {% mod ?}
        begin
          z.state^.mode := BAD;
          z.msg := 'incorrect header check';
          z.state^.sub.marker := 5;       { can't try inflateSync }
          continue;      { break C-switch }
        end;
        if ((b and PRESET_DICT) = 0) then
        begin
          z.state^.mode := BLOCKS;
    continue;      { break C-switch }
        end;
        z.state^.mode := DICT4;
        { falltrough }
      end;
    DICT4:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {z.state^.sub.check.need := uLong(NEXTBYTE(z)) shl 24;}
        Dec(z.avail_in);
        Inc(z.total_in);
        z.state^.sub.check.need :=  uLong(z.next_in^) shl 24;
        Inc(z.next_in);

        z.state^.mode := DICT3;        { falltrough }
      end;
    DICT3:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;
        {Inc(z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 16);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 16);
        Inc(z.next_in);

        z.state^.mode := DICT2;        { falltrough }
      end;
    DICT2:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        r := f;

        {Inc(z.state^.sub.check.need, uLong(NEXTBYTE(z)) shl 8);}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) shl 8);
        Inc(z.next_in);

        z.state^.mode := DICT1;        { falltrough }
      end;
    DICT1:
      begin
        if (z.avail_in = 0) then
        begin
          inflate := r;
          exit;
        end;
        { r := f;    ---  wird niemals benutzt }
        {Inc(z.state^.sub.check.need, uLong(NEXTBYTE(z)) );}
        Dec(z.avail_in);
        Inc(z.total_in);
        Inc(z.state^.sub.check.need, uLong(z.next_in^) );
        Inc(z.next_in);

        z.adler := z.state^.sub.check.need;
        z.state^.mode := DICT0;
        inflate := Z_NEED_DICT;
        exit;
      end;
    DICT0:
      begin
        z.state^.mode := BAD;
        z.msg := 'need dictionary';
        z.state^.sub.marker := 0;         { can try inflateSync }
        inflate := Z_STREAM_ERROR;
        exit;
      end;
    BAD:
      begin
        inflate := Z_DATA_ERROR;
        exit;
      end;
    else
      begin
        inflate := Z_STREAM_ERROR;
        exit;
      end;
  end;
{$ifdef NEED_DUMMY_result}
  result := Z_STREAM_ERROR;  { Some dumb compilers complain without this }
{$endif}
end;

function inflateSetDictionary(var z : z_stream;
                              dictionary : pBytef; {const array of byte}
                              dictLength : uInt) : int;
var
  length : uInt;
begin
  length := dictLength;

  if (z.state = Z_NULL) or (z.state^.mode <> DICT0) then
  begin
    inflateSetDictionary := Z_STREAM_ERROR;
    exit;
  end;
  if (adler32(Long(1), dictionary, dictLength) <> z.adler) then
  begin
    inflateSetDictionary := Z_DATA_ERROR;
    exit;
  end;
  z.adler := Long(1);

  if (length >= (uInt(1) shl z.state^.wbits)) then
  begin
    length := (1 shl z.state^.wbits)-1;
    Inc( dictionary, dictLength - length);
  end;
  inflate_set_dictionary(z.state^.blocks^, dictionary^, length);
  z.state^.mode := BLOCKS;
  inflateSetDictionary := Z_OK;
end;


function inflateSync(var z : z_stream) : int;
const
  mark : packed array[0..3] of byte = (0, 0, $ff, $ff);
var
  n : uInt;       { number of bytes to look at }
  p : pBytef;     { pointer to bytes }
  m : uInt;       { number of marker bytes found in a row }
  r, w : uLong;   { temporaries to save total_in and total_out }
begin
  { set up }
  if (z.state = Z_NULL) then
  begin
    inflateSync := Z_STREAM_ERROR;
    exit;
  end;
  if (z.state^.mode <> BAD) then
  begin
    z.state^.mode := BAD;
    z.state^.sub.marker := 0;
  end;
  n := z.avail_in;
  if (n = 0) then
  begin
    inflateSync := Z_BUF_ERROR;
    exit;
  end;
  p := z.next_in;
  m := z.state^.sub.marker;

  { search }
  while (n <> 0) and (m < 4) do
  begin
    if (p^ = mark[m]) then
      Inc(m)
    else
      if (p^ <> 0) then
        m := 0
      else
        m := 4 - m;
    Inc(p);
    Dec(n);
  end;

  { restore }
  Inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
  z.next_in := p;
  z.avail_in := n;
  z.state^.sub.marker := m;


  { return no joy or set up to restart on a new block }
  if (m <> 4) then
  begin
    inflateSync := Z_DATA_ERROR;
    exit;
  end;
  r := z.total_in;
  w := z.total_out;
  inflateReset(z);
  z.total_in := r;
  z.total_out := w;
  z.state^.mode := BLOCKS;
  inflateSync := Z_OK;
end;


{
  returns true if inflate is currently at the end of a block generated
  by Z_SYNC_FLUSH or Z_FULL_FLUSH. This function is used by one PPP
  implementation to provide an additional safety check. PPP uses Z_SYNC_FLUSH
  but removes the length bytes of the resulting empty stored block. When
  decompressing, PPP checks that at the end of input packet, inflate is
  waiting for these length bytes.
}

function inflateSyncPoint(var z : z_stream) : int;
begin
  if (z.state = Z_NULL) or (z.state^.blocks = Z_NULL) then
  begin
    inflateSyncPoint := Z_STREAM_ERROR;
    exit;
  end;
  inflateSyncPoint := inflate_blocks_sync_point(z.state^.blocks^);
end;

// from ziputils
function ALLOC(size : int) : voidp;
begin
  ALLOC := zcalloc (NIL, size, 1);
end;

procedure TRYFREE(p : voidp);
begin
  if Assigned(p) then
    zcfree(NIL, p);
end;

{ ---------------------------------------------------------------- }

function fopen(filename : PChar; mode : open_mode) : FILEptr;
var
  fp : FILEptr;
begin
  fp := NIL;
  try
    Case mode of
    fopenread: fp := TFileStream.Create(filename, fmOpenRead);
    fopenwrite: fp := TFileStream.Create(filename, fmCreate);
    fappendwrite :
      begin
        fp := TFileStream.Create(filename, fmOpenReadWrite);
        fp.Seek(soFromEnd, 0);
      end;
    end;
  except
    on EFOpenError do
      fp := NIL;
  end;
  fopen := fp;
end;

procedure fclose(fp : FILEptr);
begin
  fp.Free;
end;

function fread(buf : voidp;
               recSize : uInt;
               recCount : uInt;
               fp : FILEptr) : uInt;
var
  totalSize, readcount : uInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * uInt(recSize);
    readCount := fp.Read(buf^, totalSize);
    if (readcount <> totalSize) then
      fread := readcount div recSize
    else
      fread := recCount;
  end
  else
    fread := 0;
end;

function fwrite(buf : voidp;
                recSize : uInt;
                recCount : uInt;
                fp : FILEptr) : uInt;
var
  totalSize, written : uInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * uInt(recSize);
    written := fp.Write(buf^, totalSize);
    if (written <> totalSize) then
      fwrite := written div recSize
    else
      fwrite := recCount;
  end
  else
    fwrite := 0;
end;

function fseek(fp : FILEptr;
               recPos : uLong;
               mode : seek_mode) : int;
const
  fsmode : array[seek_mode] of Word
    = (soFromBeginning, soFromCurrent, soFromEnd);
begin
  fp.Seek(recPos, fsmode[mode]);
  fseek := 0; { = 0 for success }
end;

function ftell(fp : FILEptr) : uLong;
begin
  ftell := fp.Position;
end;

function feof(fp : FILEptr) : uInt;
begin
  feof := 0;
  if Assigned(fp) then
    if fp.Position = fp.Size then
      feof := 1
    else
      feof := 0;
end;


{ ---------------------------------------------------------------- }

{$ifdef unix and not def (CASESENSITIVITYDEFAULT_YES) and \
                      !defined(CASESENSITIVITYDEFAULT_NO)}
{$define CASESENSITIVITYDEFAULT_NO}
{$endif}


const
  UNZ_BUFSIZE = Z_BUFSIZE;
  UNZ_MAXFILENAMEINZIP = Z_MAXFILENAMEINZIP;

const
  unz_copyright : PChar = ' unzip 0.15 Copyright 1998 Gilles Vollant ';

{ unz_file_info_internal contain internal info about a file in zipfile }
type
  unz_file_info_internal = record
    offset_curfile : uLong; { relative offset of local header 4 bytes }
  end;
  unz_file_info_internal_ptr = ^unz_file_info_internal;


{ file_in_zip_read_info_s contain internal information about a file
  in zipfile, when reading and decompress it }
type
  file_in_zip_read_info_s = record
    read_buffer : PChar;  { internal buffer for compressed data }
    stream : z_stream;    { zLib stream structure for inflate }

    pos_in_zipfile : uLong;       { position in byte on the zipfile, for fseek}
    stream_initialised : boolean;   { flag set if stream structure is initialised}

    offset_local_extrafield : uLong;{ offset of the local extra field }
    size_local_extrafield : uInt;{ size of the local extra field }
    pos_local_extrafield : uLong;   { position in the local extra field in read}

    crc32 : uLong;                { crc32 of all data uncompressed }
    crc32_wait : uLong;           { crc32 we must obtain after decompress all }
    rest_read_compressed : uLong; { number of byte to be decompressed }
    rest_read_uncompressed : uLong;{number of byte to be obtained after decomp}
    afile : FILEptr;              { io structure of the zipfile }
    compression_method : uLong;   { compression method (0=store) }
    byte_before_the_zipfile : uLong;{ byte before the zipfile, (>0 for sfx) }
  end;
  file_in_zip_read_info_s_ptr = ^file_in_zip_read_info_s;


{ unz_s contain internal information about the zipfile }
type
  unz_s = record
   afile : FILEptr;                 { io structore of the zipfile }
   gi : unz_global_info;       { public global information }
   byte_before_the_zipfile : uLong;{ byte before the zipfile, (>0 for sfx)}
   num_file : uLong;             { number of the current file in the zipfile}
   pos_in_central_dir : uLong;   { pos of the current file in the central dir}
   current_file_ok : boolean;      { flag about the usability of the current file}
   central_pos : uLong;          { position of the beginning of the central dir}

   size_central_dir : uLong;     { size of the central directory  }
   offset_central_dir : uLong;   { offset of start of central directory with
                                   respect to the starting disk number }

   cur_file_info : unz_file_info; { public info about the current file in zip}
   cur_file_info_internal : unz_file_info_internal; { private info about it}
   pfile_in_zip_read : file_in_zip_read_info_s_ptr; { structure about the current
                                      file if we are decompressing it }
  end;
  unz_s_ptr = ^unz_s;


{ ===========================================================================
  Read a byte from a gz_stream; update next_in and avail_in. Return EOF
  for end of file.
  IN assertion: the stream s has been sucessfully opened for reading. }


function unzlocal_getByte(fin : FILEptr; var pi : int) : int;
var
  c : Byte;
  err : int;
begin
  err := fread(@c, 1, 1, fin);

  if (err = 1) then
  begin
    pi := int(c);
    unzlocal_getByte := UNZ_OK;
    {exit;}
  end
  else
  begin
    if feof(fin)=1 then    {if ferror(fin) then}
      unzlocal_getByte := UNZ_ERRNO
    else
      unzlocal_getByte := UNZ_EOF;
    {exit;}
  end;
end;


{ ===========================================================================
   Reads a long in LSB order from the given gz_stream. Sets }

function unzlocal_getShort (fin : FILEptr;
                            var pX : uLong) : int;
var
  x : uLong;
  i : int;
  err : int;
begin
  err := unzlocal_getByte(fin, i);
  x := uLong(i);

  if (err=UNZ_OK) then
    err := unzlocal_getByte(fin,i);
  Inc(x, uLong(i) shl 8);

  if (err=UNZ_OK) then
    pX := x
  else
    pX := 0;
  unzlocal_getShort := err;
end;

function unzlocal_getLong (fin : FILEptr; var pX : uLong) : int;
var
  x : uLong;
  i : int;
  err : int;
begin
  err := unzlocal_getByte(fin,i);
  x := uLong(i);

  if (err=UNZ_OK) then
    err := unzlocal_getByte(fin,i);
  Inc(x, uLong(i) shl 8);

  if (err=UNZ_OK) then
    err := unzlocal_getByte(fin,i);
  Inc(x, uLong(i) shl 16);

  if (err=UNZ_OK) then
    err := unzlocal_getByte(fin,i);
  Inc(x, uLong(i) shl 24);

  if (err=UNZ_OK) then
    pX := x
  else
    pX := 0;
  unzlocal_getLong := err;
end;


{ My own strcmpi / strcasecmp }
function strcmpcasenosensitive_internal (fileName1 : PChar;
                                         fileName2 : PChar) : int;
var
  c1, c2 : char;
begin
  repeat
    c1 := fileName1^; Inc(fileName1);
    c2 := fileName2^; Inc(fileName2);
    if (c1>='a') and (c1<='z') then
      Dec(c1,$20);
    if (c2>='a') and (c2<='z') then
      Dec(c2, $20);
    if (c1=#0) then
    begin
      if c2=#0 then
        strcmpcasenosensitive_internal := 0
      else
        strcmpcasenosensitive_internal := -1;
      exit;
    end;
    if (c2=#0) then
    begin
      strcmpcasenosensitive_internal := 1;
      exit;
    end;
    if (c1<c2) then
    begin
      strcmpcasenosensitive_internal := -1;
      exit;
    end;
    if (c1>c2) then
    begin
      strcmpcasenosensitive_internal := 1;
      exit;
    end;
  until false;
end;


const
  CASESENSITIVITYDEFAULTVALUE = 2;

function unzStringFileNameCompare(const fileName1 : PChar;
                                  const fileName2 : PChar;
                                  iCaseSensitivity : int) : int; { ZEXPORT }
{ Compare two filename (fileName1,fileName2).
  If iCaseSenisivity = 1 (1=true),
    comparision is case sensitive (like strcmp)
  If iCaseSenisivity = 2 (0=false),
    comparision is not case sensitive (like strcmpi or strcasecmp)
  If iCaseSenisivity = 0, case sensitivity is defaut of your
    operating system like 1 on Unix, 2 on Windows)
}
begin
  if (iCaseSensitivity=0) then
    iCaseSensitivity := CASESENSITIVITYDEFAULTVALUE;

  if (iCaseSensitivity=1) then
  begin
    unzStringFileNameCompare := strComp(fileName1,fileName2);
    exit;
  end;

  unzStringFileNameCompare := strcmpcasenosensitive_internal(fileName1,fileName2);
end;

const
  BUFREADCOMMENT = $400;

{ Locate the Central directory of a zipfile (at the end, just before
  the global comment) }

function unzlocal_SearchCentralDir(fin : FILEptr) : uLong;
var
  buf : pzByteArray;
  uSizeFile : uLong;
  uBackRead : uLong;
  uMaxBack : uLong;
  uPosFound : uLong;
var
  uReadSize,uReadPos : uLong;
  i : int;
begin
  uMaxBack := $ffff; { maximum size of global comment }
  uPosFound := 0;

  if (fseek(fin,0,SEEK_END) <> 0) then
  begin
    unzlocal_SearchCentralDir := 0;
    exit;
  end;

  uSizeFile := ftell(fin);

  if (uMaxBack>uSizeFile) then
    uMaxBack := uSizeFile;

  buf := pzByteArray(ALLOC(BUFREADCOMMENT+4));
  if (buf=NIL) then
  begin
    unzlocal_SearchCentralDir := 0;
    exit;
  end;

  uBackRead := 4;
  while (uBackRead<uMaxBack) do
  begin

    if (uBackRead+BUFREADCOMMENT>uMaxBack) then
      uBackRead := uMaxBack
    else
      Inc(uBackRead, BUFREADCOMMENT);
    uReadPos := uSizeFile-uBackRead ;

    if ((BUFREADCOMMENT+4) < (uSizeFile-uReadPos)) then
      uReadSize := (BUFREADCOMMENT+4)
    else
      uReadSize := (uSizeFile-uReadPos);

    if fseek(fin,uReadPos,SEEK_SET)<>0 then
      break;

    if fread(buf, uInt(uReadSize), 1, fin)<>1 then
      break;

    i := int(uReadSize)-3;
    while (i>0) do
    begin
      Dec(i);
      if (buf^[i] = $50) and (buf^[i+1] = $4b) and    { ENDHEADERMAGIC }
          (buf^[i+2] = $05) and (buf^[i+3] = $06) then
      begin
        uPosFound := uReadPos+uInt(i);
        break;
      end;
    end;

    if (uPosFound <> 0) then
      break;
  end;
  TRYFREE(buf);
  unzlocal_SearchCentralDir := uPosFound;
end;


{ Open a Zip file. path contain the full pathname (by example,
  on a Windows NT computer "c:\\zlib\\zlib111.zip" or on an Unix computer
  "zlib/zlib111.zip".
  If the zipfile cannot be opened (file don't exist or in not valid), the
  return value is NIL.
  Else, the return value is a unzFile Handle, usable with other function
     of this unzip package.
}

function unzOpen (const path : PChar) : unzFile; { ZEXPORT }
var
  us : unz_s;
  s  : unz_s_ptr;
  central_pos,uL : uLong;
  fin : FILEptr;

  number_disk : uLong; { number of the current dist, used for spaning ZIP,
                         unsupported, always 0 }
  number_disk_with_CD : uLong; { number the the disk with central dir,
                        used for spaning ZIP, unsupported, always 0 }
  number_entry_CD : uLong; { total number of entries in the central dir
                                 (same than number_entry on nospan) }

  err : int;
begin
  err := UNZ_OK;

  if (unz_copyright[0]<>' ') then
  begin
    unzOpen := NIL;
    exit;
  end;

  fin := fopen(path,fopenread);
  if (fin=NIL) then
  begin
    unzOpen := NIL;
    exit;
  end;

  central_pos := unzlocal_SearchCentralDir(fin);
  if (central_pos = 0) then
    err := UNZ_ERRNO;

  if (fseek(fin,central_pos,SEEK_SET) <> 0) then
    err := UNZ_ERRNO;

  { the signature, already checked }
  if (unzlocal_getLong(fin,uL) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { number of this disk }
  if (unzlocal_getShort(fin,number_disk) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { number of the disk with the start of the central directory }
  if (unzlocal_getShort(fin,number_disk_with_CD) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { total number of entries in the central dir on this disk }
  if (unzlocal_getShort(fin,us.gi.number_entry) <> UNZ_OK) then
    err := UNZ_ERRNO;

  { total number of entries in the central dir }
  if (unzlocal_getShort(fin,number_entry_CD) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if ((number_entry_CD <> us.gi.number_entry) or
    (number_disk_with_CD <> 0) or
    (number_disk <> 0)) then
    err := UNZ_BADZIPFILE;

  { size of the central directory }
  if (unzlocal_getLong(fin,us.size_central_dir)<>UNZ_OK) then
    err := UNZ_ERRNO;

  { offset of start of central directory with respect to the
        starting disk number }
  if (unzlocal_getLong(fin,us.offset_central_dir)<>UNZ_OK) then
    err := UNZ_ERRNO;

  { zipfile comment length }
  if (unzlocal_getShort(fin,us.gi.size_comment)<>UNZ_OK) then
    err := UNZ_ERRNO;

  if ((central_pos < us.offset_central_dir+us.size_central_dir) and
    (err = UNZ_OK)) then
    err := UNZ_BADZIPFILE;

  if (err<>UNZ_OK) then
  begin
    fclose(fin);
    unzOpen := NIL;
    exit;
  end;

  us.afile := fin;
  us.byte_before_the_zipfile := central_pos -
      (us.offset_central_dir + us.size_central_dir);
  us.central_pos := central_pos;
  us.pfile_in_zip_read := NIL;

  s := unz_s_ptr(ALLOC(sizeof(unz_s)));
  s^ := us;
  unzGoToFirstFile(unzFile(s));
  unzOpen := unzFile(s);
end;


{ Close a ZipFile opened with unzipOpen.
  If there are files inside the .Zip opened with unzOpenCurrentFile()
  (see later), these files MUST be closed with unzipCloseCurrentFile()
  before a call unzipClose.
  return UNZ_OK if there is no problem. }

function unzClose (afile : unzFile) : int; { ZEXPORT }
var
  s : unz_s_ptr;
begin
  if (afile=NIL) then
  begin
    unzClose := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);

  if (s^.pfile_in_zip_read<>NIL) then
    unzCloseCurrentFile(afile);

  fclose(s^.afile);
  TRYFREE(s);
  unzClose := UNZ_OK;
end;

{ Write info about the ZipFile in the pglobal_info structure.
  No preparation of the structure is needed
  return UNZ_OK if there is no problem. }

function unzGetGlobalInfo (afile : unzFile;
                       var pglobal_info : unz_global_info) : int; { ZEXPORT }
var
 s : unz_s_ptr;
begin
  if (afile=NIL) then
  begin
    unzGetGlobalInfo := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  pglobal_info := s^.gi;
  unzGetGlobalInfo := UNZ_OK;
end;


{ Translate date/time from Dos format to tm_unz (more easily readable) }
procedure unzlocal_DosDateToTmuDate (ulDosDate : uLong;
                                     var ptm : tm_unz);
var
  uDate : uLong;
begin
  uDate := uLong(ulDosDate shr 16);
  ptm.tm_mday := uInt(uDate and $1f) ;
  ptm.tm_mon :=  uInt((( (uDate) and $1E0) div $20)-1) ;
  ptm.tm_year := uInt(((uDate and $0FE00) div $0200)+1980) ;

  ptm.tm_hour := uInt ((ulDosDate and $F800) div $800);
  ptm.tm_min :=  uInt ((ulDosDate and $7E0) div $20) ;
  ptm.tm_sec :=  uInt (2*(ulDosDate and $1f)) ;
end;

{ Get Info about the current file in the zipfile, with internal only info }
function unzlocal_GetCurrentFileInfoInternal (
  afile : unzFile;
  pfile_info : unz_file_info_ptr;
  pfile_info_internal : unz_file_info_internal_ptr;
  szFileName : PChar;
  fileNameBufferSize : uLong;
  extraField : voidp;
  extraFieldBufferSize : uLong;
  szComment : PChar;
  commentBufferSize : uLong ) : int;
  var
    s : unz_s_ptr;
    file_info : unz_file_info;
    file_info_internal : unz_file_info_internal;
    err : int;
    uMagic : uLong;
    lSeek : long;
  var
    uSizeRead : uLong;
  begin
    err := UNZ_OK;
    lSeek := 0;
    if (afile = NIL) then
    begin
      unzlocal_GetCurrentFileInfoInternal := UNZ_PARAMERROR;
      exit;
    end;
    s := unz_s_ptr(afile);
    
    if (fseek(s^.afile,
      s^.pos_in_central_dir+s^.byte_before_the_zipfile,SEEK_SET)<>0) then
      err := UNZ_ERRNO;

    { we check the magic }
    if (err=UNZ_OK) then
      if (unzlocal_getLong(s^.afile, uMagic) <> UNZ_OK) then
        err := UNZ_ERRNO
      else
        if (uMagic<> CENTRALHEADERMAGIC) then
    err := UNZ_BADZIPFILE;

    if (unzlocal_getShort(s^.afile, file_info.version) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getShort(s^.afile, file_info.version_needed) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getShort(s^.afile, file_info.flag) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getShort(s^.afile, file_info.compression_method) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getLong(s^.afile, file_info.dosDate) <> UNZ_OK) then
      err := UNZ_ERRNO;

    unzlocal_DosDateToTmuDate(file_info.dosDate, file_info.tmu_date);

    if (unzlocal_getLong(s^.afile, file_info.crc) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getLong(s^.afile, file_info.compressed_size) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getLong(s^.afile, file_info.uncompressed_size) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getShort(s^.afile, file_info.size_filename) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getShort(s^.afile, file_info.size_file_extra) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getShort(s^.afile, file_info.size_file_comment) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getShort(s^.afile, file_info.disk_num_start) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getShort(s^.afile, file_info.internal_fa) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getLong(s^.afile, file_info.external_fa) <> UNZ_OK) then
      err := UNZ_ERRNO;

    if (unzlocal_getLong(s^.afile, file_info_internal.offset_curfile) <> UNZ_OK) then
      err := UNZ_ERRNO;

    Inc(lSeek, file_info.size_filename);
    if ((err=UNZ_OK) and (szFileName<>NIL)) then
    begin
      if (file_info.size_filename<fileNameBufferSize) then
      begin
        (szFileName+file_info.size_filename)^:=#0;
  uSizeRead := file_info.size_filename;
      end
      else
        uSizeRead := fileNameBufferSize;

      if (file_info.size_filename>0) and (fileNameBufferSize>0) then
      begin
        if fread(szFileName, uInt(uSizeRead),1,s^.afile)<>1 then
    err := UNZ_ERRNO;
      end;
      Dec(lSeek, uSizeRead);
    end;

    if ((err=UNZ_OK) and (extraField<>NIL)) then
    begin
      if (file_info.size_file_extra<extraFieldBufferSize) then
        uSizeRead := file_info.size_file_extra
      else
        uSizeRead := extraFieldBufferSize;

      if (lSeek<>0) then
      begin
        if (fseek(s^.afile,lSeek,SEEK_CUR)=0) then
    lSeek := 0
  else
    err := UNZ_ERRNO;
      end;

      if ((file_info.size_file_extra>0) and (extraFieldBufferSize>0)) then
      begin
        if fread(extraField, uInt(uSizeRead),1, s^.afile)<>1 then
          err := UNZ_ERRNO;
      end;
      Inc(lSeek, file_info.size_file_extra - uSizeRead);
    end
    else
      Inc(lSeek, file_info.size_file_extra);

    if ((err=UNZ_OK) and (szComment<>NIL)) then
    begin
      if (file_info.size_file_comment<commentBufferSize) then
      begin
        (szComment+file_info.size_file_comment)^ := #0;
  uSizeRead := file_info.size_file_comment;
      end
      else
        uSizeRead := commentBufferSize;

      if (lSeek<>0) then
      begin
        if (fseek(s^.afile,lSeek,SEEK_CUR)=0) then
    lSeek := 0
  else
    err := UNZ_ERRNO;
      end;
      if ((file_info.size_file_comment>0) and (commentBufferSize>0)) then
      begin
        if fread(szComment, uInt(uSizeRead),1,s^.afile)<>1 then
    err := UNZ_ERRNO;
      end;
      Inc(lSeek, file_info.size_file_comment - uSizeRead);
    end
    else
      Inc(lSeek, file_info.size_file_comment);

    if ((err=UNZ_OK) and (pfile_info<>NIL)) then
      pfile_info^ := file_info;

    if ((err=UNZ_OK) and (pfile_info_internal<>NIL)) then
      pfile_info_internal^ := file_info_internal;

    unzlocal_GetCurrentFileInfoInternal := err;
  end;


{ Write info about the ZipFile in the *pglobal_info structure.
  No preparation of the structure is needed
  return UNZ_OK if there is no problem. }

function unzGetCurrentFileInfo(afile : unzFile;
                               pfile_info : unz_file_info_ptr;
             szFileName : PChar;
             fileNameBufferSize : uLong;
             extraField : voidp;
             extraFieldBufferSize : uLong;
             szComment : PChar;
             commentBufferSize : uLong) : int; { ZEXPORT }

{ Get Info about the current file
  if pfile_info<>NIL, the pfile_info^ structure will contain somes
  info about the current file
  if szFileName<>NIL, the filemane string will be copied in szFileName
      (fileNameBufferSize is the size of the buffer)
  if extraField<>NIL, the extra field information will be copied in
    extraField  (extraFieldBufferSize is the size of the buffer).
    This is the Central-header version of the extra field
  if szComment<>NIL, the comment string of the file will be copied in
    szComment (commentBufferSize is the size of the buffer) }

begin
  unzGetCurrentFileInfo := unzlocal_GetCurrentFileInfoInternal(afile,
            pfile_info,NIL,szFileName,fileNameBufferSize, extraField,
            extraFieldBufferSize, szComment,commentBufferSize);
end;


{ Set the current file of the zipfile to the first file.
  return UNZ_OK if there is no problem }

function unzGoToFirstFile(afile : unzFile) : int;  { ZEXPORT }
var
  err : int;
  s : unz_s_ptr;
begin
  if (afile=NIL) then
  begin
    unzGoToFirstFile := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  s^.pos_in_central_dir := s^.offset_central_dir;
  s^.num_file := 0;
  err := unzlocal_GetCurrentFileInfoInternal(afile, @s^.cur_file_info,
    @s^.cur_file_info_internal, NIL,0,NIL,0,NIL,0);
  s^.current_file_ok := (err = UNZ_OK);
  unzGoToFirstFile := err;
end;


{ Set the current file of the zipfile to the next file.
  return UNZ_OK if there is no problem
  return UNZ_END_OF_LIST_OF_FILE if the actual file was the latest. }

function unzGoToNextFile(afile : unzFile) : int; { ZEXPORT }
var
  s : unz_s_ptr;
  err : int;
begin
  if (afile=NIL) then
  begin
    unzGoToNextFile := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  if not s^.current_file_ok then
  begin
    unzGoToNextFile := UNZ_END_OF_LIST_OF_FILE;
    exit;
  end;
  if (s^.num_file+1 = s^.gi.number_entry) then
  begin
    unzGoToNextFile := UNZ_END_OF_LIST_OF_FILE;
    exit;
  end;

  Inc(s^.pos_in_central_dir,
    SIZECENTRALDIRITEM + s^.cur_file_info.size_filename +
    s^.cur_file_info.size_file_extra + s^.cur_file_info.size_file_comment);
  Inc(s^.num_file);
  err := unzlocal_GetCurrentFileInfoInternal(afile, @s^.cur_file_info,
     @s^.cur_file_info_internal, NIL,0,NIL,0,NIL,0);
  s^.current_file_ok := (err = UNZ_OK);
  unzGoToNextFile := err;
end;


{ Try locate the file szFileName in the zipfile.
  For the iCaseSensitivity signification, see unzStringFileNameCompare

  return value :
  UNZ_OK if the file is found. It becomes the current file.
  UNZ_END_OF_LIST_OF_FILE if the file is not found }

function unzLocateFile(afile : unzFile;
                       const szFileName : PChar;
           iCaseSensitivity : int) : int; { ZEXPORT }
var
  s : unz_s_ptr;
  err : int;
  num_fileSaved : uLong;
  pos_in_central_dirSaved : uLong;
var
  szCurrentFileName : array[0..UNZ_MAXFILENAMEINZIP+1-1] of char;
begin
  if (afile=NIL) then
  begin
    unzLocateFile := UNZ_PARAMERROR;
    exit;
  end;

  if (strlen(szFileName)>=UNZ_MAXFILENAMEINZIP) then
  begin
    unzLocateFile := UNZ_PARAMERROR;
    exit;
  end;

  s := unz_s_ptr(afile);
  if (not s^.current_file_ok) then
  begin
    unzLocateFile := UNZ_END_OF_LIST_OF_FILE;
    exit;
  end;
  num_fileSaved := s^.num_file;
  pos_in_central_dirSaved := s^.pos_in_central_dir;

  err := unzGoToFirstFile(afile);

  while (err = UNZ_OK) do
  begin
    unzGetCurrentFileInfo(afile,NIL,
        szCurrentFileName,sizeof(szCurrentFileName)-1, NIL,0,NIL,0);
    if (unzStringFileNameCompare(szCurrentFileName,
         szFileName,iCaseSensitivity)=0) then
    begin
      unzLocateFile := UNZ_OK;
      exit;
    end;
    err := unzGoToNextFile(afile);
  end;

  s^.num_file := num_fileSaved;
  s^.pos_in_central_dir := pos_in_central_dirSaved;
  unzLocateFile := err;
end;


{ Read the local header of the current zipfile
  Check the coherency of the local header and info in the end of central
        directory about this file
  store in *piSizeVar the size of extra info in local header
        (filename and size of extra field data) }

function unzlocal_CheckCurrentFileCoherencyHeader (
                s : unz_s_ptr;
                var piSizeVar : uInt;
    var poffset_local_extrafield : uLong;
    var psize_local_extrafield : uInt) : int;
var
  uMagic,uData,uFlags : uLong;
  size_filename : uLong;
  size_extra_field : uLong;
  err : int;
begin
  err := UNZ_OK;

  piSizeVar := 0;
  poffset_local_extrafield := 0;
  psize_local_extrafield := 0;

  if (fseek(s^.afile,s^.cur_file_info_internal.offset_curfile +
    s^.byte_before_the_zipfile,SEEK_SET)<>0) then
  begin
    unzlocal_CheckCurrentFileCoherencyHeader := UNZ_ERRNO;
    exit;
  end;

  if (err=UNZ_OK) then
    if (unzlocal_getLong(s^.afile, uMagic) <> UNZ_OK) then
      err := UNZ_ERRNO
    else
      if (uMagic<> $04034b50) then
        err := UNZ_BADZIPFILE;

  if (unzlocal_getShort(s^.afile, uData) <> UNZ_OK) then
    err := UNZ_ERRNO;
{
  else
    if ((err=UNZ_OK) and (uData<>s^.cur_file_info.wVersion)) then
      err := UNZ_BADZIPFILE;
}
  if (unzlocal_getShort(s^.afile, uFlags) <> UNZ_OK) then
    err := UNZ_ERRNO;

  if (unzlocal_getShort(s^.afile, uData) <> UNZ_OK) then
    err := UNZ_ERRNO
  else
    if ((err=UNZ_OK) and (uData<>s^.cur_file_info.compression_method)) then
      err := UNZ_BADZIPFILE;

  if ((err=UNZ_OK) and (s^.cur_file_info.compression_method<>0) and
      (s^.cur_file_info.compression_method<>Z_DEFLATED)) then
    err := UNZ_BADZIPFILE;

  if (unzlocal_getLong(s^.afile, uData) <> UNZ_OK) then { date/time }
    err := UNZ_ERRNO;

  if (unzlocal_getLong(s^.afile, uData) <> UNZ_OK) then { crc }
    err := UNZ_ERRNO
  else
    if ((err=UNZ_OK) and (uData<>s^.cur_file_info.crc) and
       ((uFlags and 8)=0)) then
      err := UNZ_BADZIPFILE;

  if (unzlocal_getLong(s^.afile, uData) <> UNZ_OK) then { size compr }
    err := UNZ_ERRNO
  else
    if ((err=UNZ_OK) and (uData<>s^.cur_file_info.compressed_size) and
      ((uFlags and 8)=0)) then
      err := UNZ_BADZIPFILE;

  if (unzlocal_getLong(s^.afile, uData) <> UNZ_OK) then { size uncompr }
    err := UNZ_ERRNO
  else
    if ((err=UNZ_OK) and (uData<>s^.cur_file_info.uncompressed_size) and
      ((uFlags and 8)=0)) then
      err := UNZ_BADZIPFILE;


  if (unzlocal_getShort(s^.afile, size_filename) <> UNZ_OK) then
    err := UNZ_ERRNO
  else
    if ((err=UNZ_OK) and (size_filename<>s^.cur_file_info.size_filename)) then
      err := UNZ_BADZIPFILE;

  Inc(piSizeVar, uInt(size_filename));

  if (unzlocal_getShort(s^.afile, size_extra_field) <> UNZ_OK) then
    err := UNZ_ERRNO;
  poffset_local_extrafield := s^.cur_file_info_internal.offset_curfile +
            SIZEZIPLOCALHEADER + size_filename;
  psize_local_extrafield := uInt(size_extra_field);

  Inc(piSizeVar, uInt(size_extra_field));

  unzlocal_CheckCurrentFileCoherencyHeader := err;
end;

{ Open for reading data the current file in the zipfile.
  If there is no error, the return value is UNZ_OK. }

function unzOpenCurrentFile(afile : unzFile) : int; { ZEXPORT }
var
  err : int;
  Store : boolean;
  iSizeVar : uInt;
  s : unz_s_ptr;
  pfile_in_zip_read_info : file_in_zip_read_info_s_ptr;
  offset_local_extrafield : uLong;  { offset of the local extra field }
  size_local_extrafield : uInt;    { size of the local extra field }
begin
  err := UNZ_OK;

  if (afile=NIL) then
  begin
    unzOpenCurrentFile := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  if not s^.current_file_ok then
  begin
    unzOpenCurrentFile := UNZ_PARAMERROR;
    exit;
  end;

  if (s^.pfile_in_zip_read <> NIL) then
    unzCloseCurrentFile(afile);

  if (unzlocal_CheckCurrentFileCoherencyHeader(s, iSizeVar,
    offset_local_extrafield, size_local_extrafield)<>UNZ_OK) then
  begin
    unzOpenCurrentFile := UNZ_BADZIPFILE;
    exit;
  end;

  pfile_in_zip_read_info := file_in_zip_read_info_s_ptr(
  ALLOC(sizeof(file_in_zip_read_info_s)) );
  if (pfile_in_zip_read_info=NIL) then
  begin
    unzOpenCurrentFile := UNZ_INTERNALERROR;
    exit;
  end;

  pfile_in_zip_read_info^.read_buffer := PChar(ALLOC(UNZ_BUFSIZE));
  pfile_in_zip_read_info^.offset_local_extrafield := offset_local_extrafield;
  pfile_in_zip_read_info^.size_local_extrafield := size_local_extrafield;
  pfile_in_zip_read_info^.pos_local_extrafield := 0;

  if (pfile_in_zip_read_info^.read_buffer=NIL) then
  begin
    TRYFREE(pfile_in_zip_read_info);
    unzOpenCurrentFile := UNZ_INTERNALERROR;
    exit;
  end;

  pfile_in_zip_read_info^.stream_initialised := false;

  if ((s^.cur_file_info.compression_method<>0) and
      (s^.cur_file_info.compression_method<>Z_DEFLATED)) then
    err := UNZ_BADZIPFILE;
  Store := s^.cur_file_info.compression_method = 0;

  pfile_in_zip_read_info^.crc32_wait := s^.cur_file_info.crc;
  pfile_in_zip_read_info^.crc32 := 0;
  pfile_in_zip_read_info^.compression_method := s^.cur_file_info.compression_method;
  pfile_in_zip_read_info^.afile := s^.afile;
  pfile_in_zip_read_info^.byte_before_the_zipfile := s^.byte_before_the_zipfile;

  pfile_in_zip_read_info^.stream.total_out := 0;

  if (not Store) then
  begin
    pfile_in_zip_read_info^.stream.zalloc := NIL;
    pfile_in_zip_read_info^.stream.zfree := NIL;
    pfile_in_zip_read_info^.stream.opaque := voidpf(NIL);

    err := inflateInit2(pfile_in_zip_read_info^.stream, -MAX_WBITS);

    if (err = Z_OK) then
      pfile_in_zip_read_info^.stream_initialised := true;
        { windowBits is passed < 0 to tell that there is no zlib header.
          Note that in this case inflate *requires* an extra "dummy" byte
          after the compressed stream in order to complete decompression and
          return Z_STREAM_END.
          In unzip, i don't wait absolutely Z_STREAM_END because I known the
          size of both compressed and uncompressed data }
  end;
  pfile_in_zip_read_info^.rest_read_compressed := s^.cur_file_info.compressed_size ;
  pfile_in_zip_read_info^.rest_read_uncompressed := s^.cur_file_info.uncompressed_size ;


  pfile_in_zip_read_info^.pos_in_zipfile :=
    s^.cur_file_info_internal.offset_curfile + SIZEZIPLOCALHEADER + iSizeVar;

  pfile_in_zip_read_info^.stream.avail_in := uInt(0);


  s^.pfile_in_zip_read := pfile_in_zip_read_info;
  unzOpenCurrentFile := UNZ_OK;
end;


{ Read bytes from the current file (opened by unzOpenCurrentFile)
  buf contain buffer where data must be copied
  len the size of buf.

  return the number of byte copied if somes bytes are copied
  return 0 if the end of file was reached
  return <0 with error code if there is an error
    (UNZ_ERRNO for IO error, or zLib error for uncompress error) }

function unzReadCurrentFile(afile : unzFile;
                            buf : voidp;
          len : unsigned) : int; { ZEXPORT }

var
  err : int;
  iRead: uInt;
  s : unz_s_ptr;
  pfile_in_zip_read_info : file_in_zip_read_info_s_ptr;
var
  uReadThis : uInt;
var
  uDoCopy,i : uInt;
var
  uTotalOutBefore,uTotalOutAfter : uLong;
  bufBefore : pBytef;
  uOutThis : uLong;
  flush : int;
begin
  err := UNZ_OK;
  iRead := 0;
  if (afile=NIL) then
  begin
    unzReadCurrentFile := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info=NIL) then
  begin
    unzReadCurrentFile := UNZ_PARAMERROR;
    exit;
  end;

  if ((pfile_in_zip_read_info^.read_buffer = NIL)) then
  begin
    unzReadCurrentFile := UNZ_END_OF_LIST_OF_FILE;
    exit;
  end;

  if (len=0) then
  begin
    unzReadCurrentFile := 0;
    exit;
  end;

  pfile_in_zip_read_info^.stream.next_out := pBytef(buf);

  pfile_in_zip_read_info^.stream.avail_out := uInt(len);

  if (len>pfile_in_zip_read_info^.rest_read_uncompressed) then
      pfile_in_zip_read_info^.stream.avail_out :=
        uInt(pfile_in_zip_read_info^.rest_read_uncompressed);

  while (pfile_in_zip_read_info^.stream.avail_out>0) do
  begin
    if ((pfile_in_zip_read_info^.stream.avail_in = 0) and
        (pfile_in_zip_read_info^.rest_read_compressed>0) ) then
    begin
      uReadThis := UNZ_BUFSIZE;
      if (pfile_in_zip_read_info^.rest_read_compressed<uReadThis) then
  uReadThis := uInt(pfile_in_zip_read_info^.rest_read_compressed);
      if (uReadThis = 0) then
      begin
  unzReadCurrentFile := UNZ_EOF;
        exit;
      end;
      if (fseek(pfile_in_zip_read_info^.afile,
          pfile_in_zip_read_info^.pos_in_zipfile +
          pfile_in_zip_read_info^.byte_before_the_zipfile,SEEK_SET)<>0) then
      begin
  unzReadCurrentFile := UNZ_ERRNO;
        exit;
      end;
      if fread(pfile_in_zip_read_info^.read_buffer, uReadThis, 1,
             pfile_in_zip_read_info^.afile)<>1 then
      begin
  unzReadCurrentFile := UNZ_ERRNO;
        exit;
      end;
      Inc(pfile_in_zip_read_info^.pos_in_zipfile, uReadThis);

      Dec(pfile_in_zip_read_info^.rest_read_compressed, uReadThis);

      pfile_in_zip_read_info^.stream.next_in :=
         pBytef(pfile_in_zip_read_info^.read_buffer);
      pfile_in_zip_read_info^.stream.avail_in := uInt(uReadThis);
    end;

    if (pfile_in_zip_read_info^.compression_method=0) then
    begin
      if (pfile_in_zip_read_info^.stream.avail_out <
          pfile_in_zip_read_info^.stream.avail_in) then
        uDoCopy := pfile_in_zip_read_info^.stream.avail_out
      else
        uDoCopy := pfile_in_zip_read_info^.stream.avail_in;

      for i:=0 to uDoCopy-1 do
        pzByteArray(pfile_in_zip_read_info^.stream.next_out)^[i] :=
          pzByteArray(pfile_in_zip_read_info^.stream.next_in)^[i];

      pfile_in_zip_read_info^.crc32 := crc32(pfile_in_zip_read_info^.crc32,
    pfile_in_zip_read_info^.stream.next_out, uDoCopy);
      Dec(pfile_in_zip_read_info^.rest_read_uncompressed, uDoCopy);
      Dec(pfile_in_zip_read_info^.stream.avail_in, uDoCopy);
      Dec(pfile_in_zip_read_info^.stream.avail_out, uDoCopy);
      Inc(pfile_in_zip_read_info^.stream.next_out, uDoCopy);
      Inc(pfile_in_zip_read_info^.stream.next_in, uDoCopy);
      Inc(pfile_in_zip_read_info^.stream.total_out, uDoCopy);
      Inc(iRead, uDoCopy);
    end
    else
    begin
      flush := Z_SYNC_FLUSH;

      uTotalOutBefore := pfile_in_zip_read_info^.stream.total_out;
      bufBefore := pfile_in_zip_read_info^.stream.next_out;

      {
      if ((pfile_in_zip_read_info^.rest_read_uncompressed =
     pfile_in_zip_read_info^.stream.avail_out) and
    (pfile_in_zip_read_info^.rest_read_compressed = 0)) then
        flush := Z_FINISH;
      }
      err := inflate(pfile_in_zip_read_info^.stream,flush);

      uTotalOutAfter := pfile_in_zip_read_info^.stream.total_out;
      uOutThis := uTotalOutAfter-uTotalOutBefore;

      pfile_in_zip_read_info^.crc32 :=
        crc32(pfile_in_zip_read_info^.crc32,bufBefore, uInt(uOutThis));

      Dec(pfile_in_zip_read_info^.rest_read_uncompressed, uOutThis);

      Inc(iRead, uInt(uTotalOutAfter - uTotalOutBefore));

      if (err=Z_STREAM_END) then
      begin
        if iRead=0 then
          unzReadCurrentFile := UNZ_EOF
        else
          unzReadCurrentFile := iRead;
        exit;
      end;
      if (err<>Z_OK) then
        break;
    end;
  end; { while }

  if (err=Z_OK) then
  begin
    unzReadCurrentFile := iRead;
    exit;
  end;
  unzReadCurrentFile := err;
end;

{ Give the current position in uncompressed data }

function unztell(afile : unzFile) : z_off_t; { ZEXPORT }
var
  s : unz_s_ptr;
  pfile_in_zip_read_info : file_in_zip_read_info_s_ptr;
begin
  if (afile=NIL) then
  begin
    unztell := UNZ_PARAMERROR;
    exit;
  end;

  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info=NIL) then
  begin
    unztell := UNZ_PARAMERROR;
    exit;
  end;

  unztell := z_off_t(pfile_in_zip_read_info^.stream.total_out);
end;


{ return 1 (TRUE) if the end of file was reached, 0 elsewhere }

function unzeof(afile : unzFile) : int;
var
  s : unz_s_ptr;
  pfile_in_zip_read_info : file_in_zip_read_info_s_ptr;
begin
  if (afile=NIL) then
  begin
    unzeof := UNZ_PARAMERROR;
    exit;
  end;

  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info = NIL) then
  begin
    unzeof := UNZ_PARAMERROR;
    exit;
  end;

  if (pfile_in_zip_read_info^.rest_read_uncompressed = 0) then
    unzeof := 1
  else
    unzeof := 0;
end;


{ Read extra field from the current file (opened by unzOpenCurrentFile)
  This is the local-header version of the extra field (sometimes, there is
    more info in the local-header version than in the central-header)

  if buf=NIL, it return the size of the local extra field

  if buf<>NIL, len is the size of the buffer, the extra header is copied in
  buf.
  the return value is the number of bytes copied in buf, or (if <0)
  the error code }

function unzGetLocalExtrafield (afile : unzFile;
                                buf : voidp;
                                len : unsigned) : int;
var
  s : unz_s_ptr;
  pfile_in_zip_read_info : file_in_zip_read_info_s_ptr;
  read_now : uInt;
  size_to_read : uLong;
begin
  if (afile=NIL) then
  begin
    unzGetLocalExtrafield := UNZ_PARAMERROR;
    exit;
  end;

  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info=NIL) then
  begin
    unzGetLocalExtrafield := UNZ_PARAMERROR;
    exit;
  end;

  size_to_read := (pfile_in_zip_read_info^.size_local_extrafield -
                   pfile_in_zip_read_info^.pos_local_extrafield);

  if (buf=NIL) then
  begin
    unzGetLocalExtrafield := int(size_to_read);
    exit;
  end;

  if (len>size_to_read) then
    read_now := uInt(size_to_read)
  else
    read_now := uInt(len);

  if (read_now=0) then
  begin
    unzGetLocalExtrafield := 0;
    exit;
  end;

  if (fseek(pfile_in_zip_read_info^.afile,
            pfile_in_zip_read_info^.offset_local_extrafield +
      pfile_in_zip_read_info^.pos_local_extrafield,SEEK_SET)<>0) then
  begin
    unzGetLocalExtrafield := UNZ_ERRNO;
    exit;
  end;

  if fread(buf,uInt(size_to_read),1, pfile_in_zip_read_info^.afile)<>1 then
  begin
    unzGetLocalExtrafield := UNZ_ERRNO;
    exit;
  end;

  unzGetLocalExtrafield := int(read_now);
end;

{ Close the file in zip opened with unzOpenCurrentFile
  Return UNZ_CRCERROR if all the file was read but the CRC is not good }

function unzCloseCurrentFile(afile : unzFile) : int; { ZEXPORT }
var
  err : int;
  s : unz_s_ptr;
  pfile_in_zip_read_info : file_in_zip_read_info_s_ptr;
begin
  err := UNZ_OK;

  if (afile=NIL) then
  begin
    unzCloseCurrentFile := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);
  pfile_in_zip_read_info := s^.pfile_in_zip_read;

  if (pfile_in_zip_read_info=NIL) then
  begin
    unzCloseCurrentFile := UNZ_PARAMERROR;
    exit;
  end;


  if (pfile_in_zip_read_info^.rest_read_uncompressed = 0) then
  begin
    if (pfile_in_zip_read_info^.crc32 <> pfile_in_zip_read_info^.crc32_wait) then
      err :=UNZ_CRCERROR;
  end;


  TRYFREE(pfile_in_zip_read_info^.read_buffer);
  pfile_in_zip_read_info^.read_buffer := NIL;
  if (pfile_in_zip_read_info^.stream_initialised) then
    inflateEnd(pfile_in_zip_read_info^.stream);

  pfile_in_zip_read_info^.stream_initialised := false;
  TRYFREE(pfile_in_zip_read_info);

  s^.pfile_in_zip_read := NIL;

  unzCloseCurrentFile := err;
end;


{ Get the global comment string of the ZipFile, in the szComment buffer.
  uSizeBuf is the size of the szComment buffer.
  return the number of byte copied or an error code <0 }

function unzGetGlobalComment (afile : unzFile;
                              szComment : PChar;
            uSizeBuf : uLong) : int; { ZEXPORT }

var
  s : unz_s_ptr;
  uReadThis : uLong;
begin
  if (afile=NIL) then
  begin
    unzGetGlobalComment := UNZ_PARAMERROR;
    exit;
  end;
  s := unz_s_ptr(afile);

  uReadThis := uSizeBuf;
  if (uReadThis>s^.gi.size_comment) then
    uReadThis := s^.gi.size_comment;

  if (fseek(s^.afile,s^.central_pos+22,SEEK_SET)<>0) then
  begin
    unzGetGlobalComment := UNZ_ERRNO;
    exit;
  end;

  if (uReadThis>0) then
  begin
    szComment^ := #0;
    if fread(szComment, uInt(uReadThis), 1,s^.afile)<>1 then
    begin
      unzGetGlobalComment := UNZ_ERRNO;
      exit;
    end;
  end;

  if ((szComment <> NIL) and (uSizeBuf > s^.gi.size_comment)) then
    (szComment+s^.gi.size_comment)^ := #0;

  unzGetGlobalComment := int(uReadThis);
end;

const
  CASESENSITIVITY = 0;
  WRITEBUFFERSIZE = 1024*8;

function unzExtractFile(uf     : unzFile;
                        inFile : String;
                        stream : TStream) : Boolean;
 var
  fileInZip : packed array[0..255] of char;
  err       : Integer;
  buf       : Array[0..WriteBufferSize-1] of Byte;
  bufSize   : Integer;
  file_info : unz_file_info;
 begin
  if (unzLocateFile(uf,PChar(inFile),CaseSensitivity) <> UNZ_OK) then
   begin
    Result := False;
    exit;
   end;
  //
  err := unzGetCurrentFileInfo(uf, @file_info, fileInZip,
                               sizeof(fileInZip), NIL, 0, NIL,0);
  if (err <> UNZ_OK) then
   begin
    Result := False;
    exit;
   end;
  //
  err := unzOpenCurrentFile(uf);
  if (err <> UNZ_OK) then
   begin
    exit;
   end;
  //
  Stream.Size     := file_info.uncompressed_size;
  Stream.Position := 0;
  //
  repeat
   err := unzReadCurrentFile(uf,@buf,WriteBufferSize);
   If err < 0 then
    begin
     Result := False;
     Exit;
    end;
   If err > 0 then
     Stream.Write(buf,err);
  until (err=0);
  //
  err := unzCloseCurrentFile (uf);
  if (err <> UNZ_OK) then Result := False;
  //
  Result := True;
 end;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

Function ZipExtract( zipfile, infile : string; outfile : TStream ) : Boolean;
  Var
   uz : unzFile;
 begin
   uz := unzOpen(pchar(zipfile));
   if uz = nil then
    begin
     result := false;
     exit;
    end;
   Result := unzExtractFile(uz,infile,outfile);
   unzClose(uz);
 end;


end.


