{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe
Author(s):    TurboPower Software
							St�phane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALCipher (Private Key Encryption/Decryption Primitives)
Version:      4.00

Description:  Delphi library for cryptography. It provides support for
              AES, Blowfish, SHA, MD5

Legal issues: Copyright (C) 1999-2012 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     01/12/2006: add blowFish functions
              10/10/2009: add AES (Rijndael) function.
                          also move the ALCipherStringHashxxx to
                          the unit alcncrypt and rename it in
                          ALStringHashxxx
              26/06/2012: Add xe2 support

Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALCipher;

interface

uses Windows,
     Classes,
     AlFcnString;

const

  { largest structure that can be created }
  cALCipherMaxStructSize = 1024 * 2000000; {2G}


type

  { Cipher exception }
  EALCipherException = class(EALException);

  { general structures }
  pALCipherLongIntArray = ^TALCipherLongIntArray;
  TALCipherLongIntArray = array [0..cALCipherMaxStructSize div SizeOf(LongInt) - 1] of LongInt;

  TALCipherLongIntRec = packed record
    case Byte of
      1: (Lo: Word;
          Hi: Word);
      2: (LoLo: Byte;
          LoHi: Byte;
          HiLo: Byte;
          HiHi: Byte);
  end;

  TALCipherInt64 = packed record
    case Byte of
      0: (Lo: LongInt;
          Hi: LongInt);
      1: (LoLo: Word;
          LoHi: Word;
          HiLo: Word;
          HiHi: Word);
      2: (LoLoLo: Byte;
          LoLoHi: Byte;
          LoHiLo: Byte;
          LoHiHi: Byte;
          HiLoLo: Byte;
          HiLoHi: Byte;
          HiHiLo: Byte;
          HiHiHi: Byte);
  end;

  TALCipherRDLVector = record
    case Byte of
      0 : (dw : DWord);
      1 : (bt : array[0..3] of Byte);
    end;


{ encryption key types }
type
  PALCipherKey64  = ^TALCipherKey64;                                                 {!!.03}
  TALCipherKey64  = array [0..7] of Byte;

  PALCipherKey128 = ^TALCipherKey128;                                                {!!.03}
  TALCipherKey128 = array [0..15] of Byte;

  PALCipherKey192 = ^TALCipherKey192;                                                {!!.03}
  TALCipherKey192 = array [0..23] of Byte;

  PALCipherKey256 = ^TALCipherKey256;                                                {!!.03}
  TALCipherKey256 = array [0..31] of Byte;


{ encryption block types }
type
  TALCipherBFBlock   = array[0..1] of LongInt;     { BlowFish }
  TALCipherRDLBlock  = array[0..15] of Byte;       { Rijndael }


{ context type constants }
const
  cALCipherBFRounds = 16;      { 16 blowfish rounds }
  cALCipherMaxRDLRounds = 14;  { 14 Rijndael rounds }


{ block cipher context types }
type
  { Blowfish }
  TALCipherBFContext = packed record
    PBox    : array[0..(cALCipherBFRounds+1)] of LongInt;
    SBox    : array[0..3, 0..255] of LongInt;
  end;

  { Rijndael }
  TALCipherRDLContext = packed record
    Encrypt : Boolean;
    Dummy   : array[0..2] of Byte; {filler}
    Rounds  : DWord;
    case Byte of
      0 : (W  : array[0..(cALCipherMaxRDLRounds * 4)] of TALCipherRDLVector);
      1 : (Rk : array[0..cALCipherMaxRDLRounds] of TALCipherRDLBlock);
    end;

{ message digest blocks }
type
  TALCipherMD5Digest  = array [0..15] of Byte;         { 128 bits - MD5 }
  TALCipherSHA1Digest = array [0..19] of Byte;         { 160 bits - SHA-1 }


{ message digest context types }
type
  TALCipherMD5Context  = array [0..87] of Byte;        { MD5 }
  TALCipherSHA1Context = record                        { SHA-1 }
    sdHi    : DWord;
    sdLo    : DWord;
    sdIndex : DWord;
    sdHash  : array [0..4] of DWord;
    sdBuf   : array [0..63] of Byte;
  end;

{ Key generation }
procedure ALCipherGenerateMD5Key(var Key : TALCipherKey128; const Str : AnsiString);
procedure ALCipherGenerateRandomKey(var Key; KeySize : Integer);

{ Misc public utilities }
function  AlCipherBufferToHex(const Buf; BufSize : Cardinal) : AnsiString;
procedure ALCipherXorMem(var Mem1;  const Mem2;  Count : Cardinal);
function  ALCipherRolX(I, C : DWord) : DWord; register;

{ Xor }
function  ALXorEncrypt(const InString:AnsiString; StartKey,MultKey,AddKey:Integer): AnsiString;
function  ALXorDecrypt(const InString:AnsiString; StartKey,MultKey,AddKey:Integer): AnsiString;

{ Blowfish }
procedure ALCipherInitEncryptBF(Key : TALCipherKey128; var Context : TALCipherBFContext);
procedure ALCipherEncryptBF(const Context : TALCipherBFContext; var Block : TALCipherBFBlock; Encrypt : Boolean);
procedure ALCipherEncryptBFCBC(const Context : TALCipherBFContext; const Prev : TALCipherBFBlock; var Block : TALCipherBFBlock; Encrypt : Boolean);
procedure ALBFEncryptString(const InString: AnsiString; var OutString : AnsiString; const Key : TALCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptStringCBC(const InString: AnsiString; var OutString : AnsiString; const Key : TALCipherKey128; Encrypt : Boolean); overload;
function  AlBFEncryptString(const InString: AnsiString; const Key : TALCipherKey128; Encrypt : Boolean) : AnsiString; overload;
function  ALBFEncryptStringCBC(const InString: AnsiString; const Key : TALCipherKey128; Encrypt : Boolean) : AnsiString; overload;
procedure ALBFEncryptFile(const InFile, OutFile : AnsiString; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptFileCBC(const InFile, OutFile : AnsiString; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptStream(InStream, OutStream : TStream; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptStreamCBC(InStream, OutStream : TStream; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptString(const InString: AnsiString; var OutString : AnsiString; const Key: AnsiString; Encrypt : Boolean); overload;
procedure ALBFEncryptStringCBC(const InString: AnsiString; var OutString : AnsiString; const Key: AnsiString; Encrypt : Boolean); overload;
function  AlBFEncryptString(const InString: AnsiString; const Key: AnsiString; Encrypt : Boolean) : AnsiString; overload;
function  ALBFEncryptStringCBC(const InString: AnsiString; const Key: AnsiString; Encrypt : Boolean) : AnsiString; overload;
procedure ALBFEncryptStream(InStream, OutStream: TStream; const Key: AnsiString; Encrypt : Boolean); overload;
procedure ALBFEncryptStreamCBC(InStream, OutStream: TStream; const Key: AnsiString; Encrypt : Boolean); overload;
procedure ALBFEncryptFile(const InFile, OutFile: AnsiString; const Key: AnsiString; Encrypt : Boolean); overload;
procedure ALBFEncryptFileCBC(const InFile, OutFile: AnsiString; const Key: AnsiString; Encrypt : Boolean); overload;

{ Rijndael (AES) }
procedure ALCipherInitEncryptRDL(const Key; KeySize : Longint; var Context : TALCipherRDLContext; Encrypt : Boolean);
procedure ALCipherEncryptRDL(const Context : TALCipherRDLContext; var Block : TALCipherRDLBlock);
procedure ALCipherEncryptRDLCBC(const Context : TALCipherRDLContext; const Prev : TALCipherRDLBlock; var Block : TALCipherRDLBlock);
procedure ALRDLEncryptString(const InString: AnsiString; var OutString : AnsiString; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptStringCBC(const InString: AnsiString; var OutString : AnsiString; const Key; KeySize : Longint; Encrypt : Boolean); overload;
function  ALRDLEncryptString(const InString: AnsiString; const Key; KeySize : Longint; Encrypt : Boolean) : AnsiString; overload;
function  ALRDLEncryptStringCBC(const InString: AnsiString; const Key; KeySize : Longint; Encrypt : Boolean) : AnsiString; overload;
procedure ALRDLEncryptFile(const InFile, OutFile : AnsiString; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptFileCBC(const InFile, OutFile : AnsiString; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptStream(InStream, OutStream : TStream; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptStreamCBC(InStream, OutStream : TStream; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptString(const InString: AnsiString; var OutString : AnsiString; const Key: AnsiString; Encrypt : Boolean); overload;
procedure ALRDLEncryptStringCBC(const InString: AnsiString; var OutString : AnsiString; const Key: AnsiString; Encrypt : Boolean); overload;
function  ALRDLEncryptString(const InString: AnsiString; const Key: AnsiString; Encrypt : Boolean) : AnsiString; overload;
function  ALRDLEncryptStringCBC(const InString: AnsiString; const Key: AnsiString; Encrypt : Boolean) : AnsiString; overload;
procedure ALRDLEncryptFile(const InFile, OutFile : AnsiString; const Key: AnsiString; Encrypt : Boolean); overload;
procedure ALRDLEncryptFileCBC(const InFile, OutFile : AnsiString; const Key: AnsiString; Encrypt : Boolean); overload;
procedure ALRDLEncryptStream(InStream, OutStream : TStream; const Key: AnsiString; Encrypt : Boolean); overload;
procedure ALRDLEncryptStreamCBC(InStream, OutStream : TStream; const Key: AnsiString; Encrypt : Boolean); overload;

{ MD5 }
procedure ALCipherInitMD5(var Context : TALCipherMD5Context);
procedure ALCipherHashMD5(var Digest : TALCipherMD5Digest; const Buf; BufSize : LongInt);
procedure ALCipherFinalizeMD5(var Context : TALCipherMD5Context; var Digest : TALCipherMD5Digest);
procedure ALCipherUpdateMD5(var Context : TALCipherMD5Context; const Buf;  BufSize : LongInt);
procedure ALFileHashMD5(var Digest : TAlCipherMD5Digest; const AFileName : AnsiString); overload;
function  ALFileHashMD5(const AFileName : AnsiString): AnsiString; overload;
procedure ALStreamHashMD5(var Digest : TAlCipherMD5Digest; AStream : TStream); overload;
function  ALStreamHashMD5(AStream : TStream): AnsiString; overload;
procedure ALStringHashMD5(var Digest : TALCipherMD5Digest; const Str : AnsiString); overload;
function  ALStringHashMD5(const Str : AnsiString): AnsiString; overload;

{ SHA1 }
procedure ALCipherInitSHA1(var Context: TALCipherSHA1Context);
procedure ALCipherHashSHA1(var Digest : TALCipherSHA1Digest; const Buf; BufSize : Longint);
procedure ALCipherUpdateSHA1(var Context : TALCipherSHA1Context; const Buf; BufSize: Longint);
procedure ALCipherFinalizeSHA1(var Context: TALCipherSHA1Context; var Digest : TALCipherSHA1Digest);
procedure ALFileHashSHA1(var Digest : TAlCipherSHA1Digest; const AFileName : AnsiString); overload;
function  ALFileHashSHA1(const AFileName : AnsiString): AnsiString; overload;
procedure ALStreamHashSHA1(var Digest : TAlCipherSHA1Digest; AStream : TStream); overload;
function  ALStreamHashSHA1(AStream : TStream): AnsiString; overload;
procedure ALStringHashSHA1(var Digest : TALCipherSHA1Digest; const Str : AnsiString); overload;
function  ALStringHashSHA1(const Str : AnsiString): AnsiString; overload;

{ Miscellaneous hash algorithms }
procedure ALCipherHashELF(var Digest : LongInt; const Buf;  BufSize : LongInt);
procedure ALCipherHashMix128(var Digest : LongInt; const Buf;  BufSize : LongInt);
procedure ALStringHashELF(var Digest : LongInt; const Str : AnsiString); overload;
function  ALStringHashELF(const Str : AnsiString): LongInt; overload;
procedure ALStringHashMix128(var Digest : LongInt; const Str : AnsiString); overload;
function  ALStringHashMix128(const Str : AnsiString): LongInt; overload;


implementation

uses sysutils,
     MMSystem,
     Math;

type
  pALCipherMD5ContextEx = ^TALCipherMD5ContextEx;
  TALCipherMD5ContextEx = packed record
    Count : array [0..1] of DWord;  {number of bits handled mod 2^64}
    State : array [0..3] of DWord;  {scratch buffer}
    Buf   : array [0..63] of Byte;    {input buffer}
  end;

type
  {bit mixing types}
  TALCipher128Bit     = array [0..3] of DWord;
  TALCipher256Bit     = array [0..7] of DWord;

type
  TALCipherBFBlockEx = packed record
    Xl : array[0..3] of Byte;
    Xr : array[0..3] of Byte;
  end;

{-Blowfish lookup tables}

const
 cALCipherbf_P: array[0..(cALCipherBFRounds + 1)] of DWord = (
  $243F6A88, $85A308D3, $13198A2E, $03707344,
  $A4093822, $299F31D0, $082EFA98, $EC4E6C89,
  $452821E6, $38D01377, $BE5466CF, $34E90C6C,
  $C0AC29B7, $C97C50DD, $3F84D5B5, $B5470917,
  $9216D5D9, $8979FB1B);

const
 cALCipherbf_S: array[0..3, 0..255] of DWord =
(
( $D1310BA6, $98DFB5AC, $2FFD72DB, $D01ADFB7,
  $B8E1AFED, $6A267E96, $BA7C9045, $F12C7F99,
  $24A19947, $B3916CF7, $0801F2E2, $858EFC16,
  $636920D8, $71574E69, $A458FEA3, $F4933D7E,

  $0D95748F, $728EB658, $718BCD58, $82154AEE,
  $7B54A41D, $C25A59B5, $9C30D539, $2AF26013,
  $C5D1B023, $286085F0, $CA417918, $B8DB38EF,
  $8E79DCB0, $603A180E, $6C9E0E8B, $B01E8A3E,

  $D71577C1, $BD314B27, $78AF2FDA, $55605C60,
  $E65525F3, $AA55AB94, $57489862, $63E81440,
  $55CA396A, $2AAB10B6, $B4CC5C34, $1141E8CE,
  $A15486AF, $7C72E993, $B3EE1411, $636FBC2A,

  $2BA9C55D, $741831F6, $CE5C3E16, $9B87931E,
  $AFD6BA33, $6C24CF5C, $7A325381, $28958677,
  $3B8F4898, $6B4BB9AF, $C4BFE81B, $66282193,
  $61D809CC, $FB21A991, $487CAC60, $5DEC8032,

  $EF845D5D, $E98575B1, $DC262302, $EB651B88,
  $23893E81, $D396ACC5, $0F6D6FF3, $83F44239,
  $2E0B4482, $A4842004, $69C8F04A, $9E1F9B5E,
  $21C66842, $F6E96C9A, $670C9C61, $ABD388F0,

  $6A51A0D2, $D8542F68, $960FA728, $AB5133A3,
  $6EEF0B6C, $137A3BE4, $BA3BF050, $7EFB2A98,
  $A1F1651D, $39AF0176, $66CA593E, $82430E88,
  $8CEE8619, $456F9FB4, $7D84A5C3, $3B8B5EBE,

  $E06F75D8, $85C12073, $401A449F, $56C16AA6,
  $4ED3AA62, $363F7706, $1BFEDF72, $429B023D,
  $37D0D724, $D00A1248, $DB0FEAD3, $49F1C09B,
  $075372C9, $80991B7B, $25D479D8, $F6E8DEF7,

  $E3FE501A, $B6794C3B, $976CE0BD, $04C006BA,
  $C1A94FB6, $409F60C4, $5E5C9EC2, $196A2463,
  $68FB6FAF, $3E6C53B5, $1339B2EB, $3B52EC6F,
  $6DFC511F, $9B30952C, $CC814544, $AF5EBD09,

  $BEE3D004, $DE334AFD, $660F2807, $192E4BB3,
  $C0CBA857, $45C8740F, $D20B5F39, $B9D3FBDB,
  $5579C0BD, $1A60320A, $D6A100C6, $402C7279,
  $679F25FE, $FB1FA3CC, $8EA5E9F8, $DB3222F8,

  $3C7516DF, $FD616B15, $2F501EC8, $AD0552AB,
  $323DB5FA, $FD238760, $53317B48, $3E00DF82,
  $9E5C57BB, $CA6F8CA0, $1A87562E, $DF1769DB,
  $D542A8F6, $287EFFC3, $AC6732C6, $8C4F5573,

  $695B27B0, $BBCA58C8, $E1FFA35D, $B8F011A0,
  $10FA3D98, $FD2183B8, $4AFCB56C, $2DD1D35B,
  $9A53E479, $B6F84565, $D28E49BC, $4BFB9790,
  $E1DDF2DA, $A4CB7E33, $62FB1341, $CEE4C6E8,

  $EF20CADA, $36774C01, $D07E9EFE, $2BF11FB4,
  $95DBDA4D, $AE909198, $EAAD8E71, $6B93D5A0,
  $D08ED1D0, $AFC725E0, $8E3C5B2F, $8E7594B7,
  $8FF6E2FB, $F2122B64, $8888B812, $900DF01C,

  $4FAD5EA0, $688FC31C, $D1CFF191, $B3A8C1AD,
  $2F2F2218, $BE0E1777, $EA752DFE, $8B021FA1,
  $E5A0CC0F, $B56F74E8, $18ACF3D6, $CE89E299,
  $B4A84FE0, $FD13E0B7, $7CC43B81, $D2ADA8D9,

  $165FA266, $80957705, $93CC7314, $211A1477,
  $E6AD2065, $77B5FA86, $C75442F5, $FB9D35CF,
  $EBCDAF0C, $7B3E89A0, $D6411BD3, $AE1E7E49,
  $00250E2D, $2071B35E, $226800BB, $57B8E0AF,

  $2464369B, $F009B91E, $5563911D, $59DFA6AA,
  $78C14389, $D95A537F, $207D5BA2, $02E5B9C5,
  $83260376, $6295CFA9, $11C81968, $4E734A41,
  $B3472DCA, $7B14A94A, $1B510052, $9A532915,

  $D60F573F, $BC9BC6E4, $2B60A476, $81E67400,
  $08BA6FB5, $571BE91F, $F296EC6B, $2A0DD915,
  $B6636521, $E7B9F9B6, $FF34052E, $C5855664,
  $53B02D5D, $A99F8FA1, $08BA4799, $6E85076A),
  {SECOND 256}
 ($4B7A70E9, $B5B32944, $DB75092E, $C4192623,
  $AD6EA6B0, $49A7DF7D, $9CEE60B8, $8FEDB266,
  $ECAA8C71, $699A17FF, $5664526C, $C2B19EE1,
  $193602A5, $75094C29, $A0591340, $E4183A3E,

  $3F54989A, $5B429D65, $6B8FE4D6, $99F73FD6,
  $A1D29C07, $EFE830F5, $4D2D38E6, $F0255DC1,
  $4CDD2086, $8470EB26, $6382E9C6, $021ECC5E,
  $09686B3F, $3EBAEFC9, $3C971814, $6B6A70A1,

  $687F3584, $52A0E286, $B79C5305, $AA500737,
  $3E07841C, $7FDEAE5C, $8E7D44EC, $5716F2B8,
  $B03ADA37, $F0500C0D, $F01C1F04, $0200B3FF,
  $AE0CF51A, $3CB574B2, $25837A58, $DC0921BD,

  $D19113F9, $7CA92FF6, $94324773, $22F54701,
  $3AE5E581, $37C2DADC, $C8B57634, $9AF3DDA7,
  $A9446146, $0FD0030E, $ECC8C73E, $A4751E41,
  $E238CD99, $3BEA0E2F, $3280BBA1, $183EB331,

  $4E548B38, $4F6DB908, $6F420D03, $F60A04BF,
  $2CB81290, $24977C79, $5679B072, $BCAF89AF,
  $DE9A771F, $D9930810, $B38BAE12, $DCCF3F2E,
  $5512721F, $2E6B7124, $501ADDE6, $9F84CD87,

  $7A584718, $7408DA17, $BC9F9ABC, $E94B7D8C,
  $EC7AEC3A, $DB851DFA, $63094366, $C464C3D2,
  $EF1C1847, $3215D908, $DD433B37, $24C2BA16,
  $12A14D43, $2A65C451, $50940002, $133AE4DD,

  $71DFF89E, $10314E55, $81AC77D6, $5F11199B,
  $043556F1, $D7A3C76B, $3C11183B, $5924A509,
  $F28FE6ED, $97F1FBFA, $9EBABF2C, $1E153C6E,
  $86E34570, $EAE96FB1, $860E5E0A, $5A3E2AB3,

  $771FE71C, $4E3D06FA, $2965DCB9, $99E71D0F,
  $803E89D6, $5266C825, $2E4CC978, $9C10B36A,
  $C6150EBA, $94E2EA78, $A5FC3C53, $1E0A2DF4,
  $F2F74EA7, $361D2B3D, $1939260F, $19C27960,

  $5223A708, $F71312B6, $EBADFE6E, $EAC31F66,
  $E3BC4595, $A67BC883, $B17F37D1, $018CFF28,
  $C332DDEF, $BE6C5AA5, $65582185, $68AB9802,
  $EECEA50F, $DB2F953B, $2AEF7DAD, $5B6E2F84,

  $1521B628, $29076170, $ECDD4775, $619F1510,
  $13CCA830, $EB61BD96, $0334FE1E, $AA0363CF,
  $B5735C90, $4C70A239, $D59E9E0B, $CBAADE14,
  $EECC86BC, $60622CA7, $9CAB5CAB, $B2F3846E,

  $648B1EAF, $19BDF0CA, $A02369B9, $655ABB50,
  $40685A32, $3C2AB4B3, $319EE9D5, $C021B8F7,
  $9B540B19, $875FA099, $95F7997E, $623D7DA8,
  $F837889A, $97E32D77, $11ED935F, $16681281,

  $0E358829, $C7E61FD6, $96DEDFA1, $7858BA99,
  $57F584A5, $1B227263, $9B83C3FF, $1AC24696,
  $CDB30AEB, $532E3054, $8FD948E4, $6DBC3128,
  $58EBF2EF, $34C6FFEA, $FE28ED61, $EE7C3C73,

  $5D4A14D9, $E864B7E3, $42105D14, $203E13E0,
  $45EEE2B6, $A3AAABEA, $DB6C4F15, $FACB4FD0,
  $C742F442, $EF6ABBB5, $654F3B1D, $41CD2105,
  $D81E799E, $86854DC7, $E44B476A, $3D816250,

  $CF62A1F2, $5B8D2646, $FC8883A0, $C1C7B6A3,
  $7F1524C3, $69CB7492, $47848A0B, $5692B285,
  $095BBF00, $AD19489D, $1462B174, $23820E00,
  $58428D2A, $0C55F5EA, $1DADF43E, $233F7061,

  $3372F092, $8D937E41, $D65FECF1, $6C223BDB,
  $7CDE3759, $CBEE7460, $4085F2A7, $CE77326E,
  $A6078084, $19F8509E, $E8EFD855, $61D99735,
  $A969A7AA, $C50C06C2, $5A04ABFC, $800BCADC,

  $9E447A2E, $C3453484, $FDD56705, $0E1E9EC9,
  $DB73DBD3, $105588CD, $675FDA79, $E3674340,
  $C5C43465, $713E38D8, $3D28F89E, $F16DFF20,
  $153E21E7, $8FB03D4A, $E6E39F2B, $DB83ADF7),
  {THIRD 256}
 ($E93D5A68, $948140F7, $F64C261C, $94692934,
  $411520F7, $7602D4F7, $BCF46B2E, $D4A20068,
  $D4082471, $3320F46A, $43B7D4B7, $500061AF,
  $1E39F62E, $97244546, $14214F74, $BF8B8840,

  $4D95FC1D, $96B591AF, $70F4DDD3, $66A02F45,
  $BFBC09EC, $03BD9785, $7FAC6DD0, $31CB8504,
  $96EB27B3, $55FD3941, $DA2547E6, $ABCA0A9A,
  $28507825, $530429F4, $0A2C86DA, $E9B66DFB,

  $68DC1462, $D7486900, $680EC0A4, $27A18DEE,
  $4F3FFEA2, $E887AD8C, $B58CE006, $7AF4D6B6,
  $AACE1E7C, $D3375FEC, $CE78A399, $406B2A42,
  $20FE9E35, $D9F385B9, $EE39D7AB, $3B124E8B,

  $1DC9FAF7, $4B6D1856, $26A36631, $EAE397B2,
  $3A6EFA74, $DD5B4332, $6841E7F7, $CA7820FB,
  $FB0AF54E, $D8FEB397, $454056AC, $BA489527,
  $55533A3A, $20838D87, $FE6BA9B7, $D096954B,

  $55A867BC, $A1159A58, $CCA92963, $99E1DB33,
  $A62A4A56, $3F3125F9, $5EF47E1C, $9029317C,
  $FDF8E802, $04272F70, $80BB155C, $05282CE3,
  $95C11548, $E4C66D22, $48C1133F, $C70F86DC,

  $07F9C9EE, $41041F0F, $404779A4, $5D886E17,
  $325F51EB, $D59BC0D1, $F2BCC18F, $41113564,
  $257B7834, $602A9C60, $DFF8E8A3, $1F636C1B,
  $0E12B4C2, $02E1329E, $AF664FD1, $CAD18115,

  $6B2395E0, $333E92E1, $3B240B62, $EEBEB922,
  $85B2A20E, $E6BA0D99, $DE720C8C, $2DA2F728,
  $D0127845, $95B794FD, $647D0862, $E7CCF5F0,
  $5449A36F, $877D48FA, $C39DFD27, $F33E8D1E,

  $0A476341, $992EFF74, $3A6F6EAB, $F4F8FD37,
  $A812DC60, $A1EBDDF8, $991BE14C, $DB6E6B0D,
  $C67B5510, $6D672C37, $2765D43B, $DCD0E804,
  $F1290DC7, $CC00FFA3, $B5390F92, $690FED0B,

  $667B9FFB, $CEDB7D9C, $A091CF0B, $D9155EA3,
  $BB132F88, $515BAD24, $7B9479BF, $763BD6EB,
  $37392EB3, $CC115979, $8026E297, $F42E312D,
  $6842ADA7, $C66A2B3B, $12754CCC, $782EF11C,

  $6A124237, $B79251E7, $06A1BBE6, $4BFB6350,
  $1A6B1018, $11CAEDFA, $3D25BDD8, $E2E1C3C9,
  $44421659, $0A121386, $D90CEC6E, $D5ABEA2A,
  $64AF674E, $DA86A85F, $BEBFE988, $64E4C3FE,

  $9DBC8057, $F0F7C086, $60787BF8, $6003604D,
  $D1FD8346, $F6381FB0, $7745AE04, $D736FCCC,
  $83426B33, $F01EAB71, $B0804187, $3C005E5F,
  $77A057BE, $BDE8AE24, $55464299, $BF582E61,

  $4E58F48F, $F2DDFDA2, $F474EF38, $8789BDC2,
  $5366F9C3, $C8B38E74, $B475F255, $46FCD9B9,
  $7AEB2661, $8B1DDF84, $846A0E79, $915F95E2,
  $466E598E, $20B45770, $8CD55591, $C902DE4C,

  $B90BACE1, $BB8205D0, $11A86248, $7574A99E,
  $B77F19B6, $E0A9DC09, $662D09A1, $C4324633,
  $E85A1F02, $09F0BE8C, $4A99A025, $1D6EFE10,
  $1AB93D1D, $0BA5A4DF, $A186F20F, $2868F169,

  $DCB7DA83, $573906FE, $A1E2CE9B, $4FCD7F52,
  $50115E01, $A70683FA, $A002B5C4, $0DE6D027,
  $9AF88C27, $773F8641, $C3604C06, $61A806B5,
  $F0177A28, $C0F586E0, $006058AA, $30DC7D62,

  $11E69ED7, $2338EA63, $53C2DD94, $C2C21634,
  $BBCBEE56, $90BCB6DE, $EBFC7DA1, $CE591D76,
  $6F05E409, $4B7C0188, $39720A3D, $7C927C24,
  $86E3725F, $724D9DB9, $1AC15BB4, $D39EB8FC,

  $ED545578, $08FCA5B5, $D83D7CD3, $4DAD0FC4,
  $1E50EF5E, $B161E6F8, $A28514D9, $6C51133C,
  $6FD5C7E7, $56E14EC4, $362ABFCE, $DDC6C837,
  $D79A3234, $92638212, $670EFA8E, $406000E0),
  {FOURTH 256}
 ($3A39CE37, $D3FAF5CF, $ABC27737, $5AC52D1B,
  $5CB0679E, $4FA33742, $D3822740, $99BC9BBE,
  $D5118E9D, $BF0F7315, $D62D1C7E, $C700C47B,
  $B78C1B6B, $21A19045, $B26EB1BE, $6A366EB4,

  $5748AB2F, $BC946E79, $C6A376D2, $6549C2C8,
  $530FF8EE, $468DDE7D, $D5730A1D, $4CD04DC6,
  $2939BBDB, $A9BA4650, $AC9526E8, $BE5EE304,
  $A1FAD5F0, $6A2D519A, $63EF8CE2, $9A86EE22,

  $C089C2B8, $43242EF6, $A51E03AA, $9CF2D0A4,
  $83C061BA, $9BE96A4D, $8FE51550, $BA645BD6,
  $2826A2F9, $A73A3AE1, $4BA99586, $EF5562E9,
  $C72FEFD3, $F752F7DA, $3F046F69, $77FA0A59,

  $80E4A915, $87B08601, $9B09E6AD, $3B3EE593,
  $E990FD5A, $9E34D797, $2CF0B7D9, $022B8B51,
  $96D5AC3A, $017DA67D, $D1CF3ED6, $7C7D2D28,
  $1F9F25CF, $ADF2B89B, $5AD6B472, $5A88F54C,

  $E029AC71, $E019A5E6, $47B0ACFD, $ED93FA9B,
  $E8D3C48D, $283B57CC, $F8D56629, $79132E28,
  $785F0191, $ED756055, $F7960E44, $E3D35E8C,
  $15056DD4, $88F46DBA, $03A16125, $0564F0BD,

  $C3EB9E15, $3C9057A2, $97271AEC, $A93A072A,
  $1B3F6D9B, $1E6321F5, $F59C66FB, $26DCF319,
  $7533D928, $B155FDF5, $03563482, $8ABA3CBB,
  $28517711, $C20AD9F8, $ABCC5167, $CCAD925F,

  $4DE81751, $3830DC8E, $379D5862, $9320F991,
  $EA7A90C2, $FB3E7BCE, $5121CE64, $774FBE32,
  $A8B6E37E, $C3293D46, $48DE5369, $6413E680,
  $A2AE0810, $DD6DB224, $69852DFD, $09072166,

  $B39A460A, $6445C0DD, $586CDECF, $1C20C8AE,
  $5BBEF7DD, $1B588D40, $CCD2017F, $6BB4E3BB,
  $DDA26A7E, $3A59FF45, $3E350A44, $BCB4CDD5,
  $72EACEA8, $FA6484BB, $8D6612AE, $BF3C6F47,

  $D29BE463, $542F5D9E, $AEC2771B, $F64E6370,
  $740E0D8D, $E75B1357, $F8721671, $AF537D5D,
  $4040CB08, $4EB4E2CC, $34D2466A, $0115AF84,
  $E1B00428, $95983A1D, $06B89FB4, $CE6EA048,

  $6F3F3B82, $3520AB82, $011A1D4B, $277227F8,
  $611560B1, $E7933FDC, $BB3A792B, $344525BD,
  $A08839E1, $51CE794B, $2F32C9B7, $A01FBAC9,
  $E01CC87E, $BCC7D1F6, $CF0111C3, $A1E8AAC7,

  $1A908749, $D44FBD9A, $D0DADECB, $D50ADA38,
  $0339C32A, $C6913667, $8DF9317C, $E0B12B4F,
  $F79E59B7, $43F5BB3A, $F2D519FF, $27D9459C,
  $BF97222C, $15E6FC2A, $0F91FC71, $9B941525,

  $FAE59361, $CEB69CEB, $C2A86459, $12BAA8D1,
  $B6C1075E, $E3056A0C, $10D25065, $CB03A442,
  $E0EC6E0E, $1698DB3B, $4C98A0BE, $3278E964,
  $9F1F9532, $E0D392DF, $D3A0342B, $8971F21E,

  $1B0A7441, $4BA3348C, $C5BE7120, $C37632D8,
  $DF359F8D, $9B992F2E, $E60B6F47, $0FE3F11D,
  $E54CDA54, $1EDAD891, $CE6279CF, $CD3E7E6F,
  $1618B166, $FD2C1D05, $848FD2C5, $F6FB2299,

  $F523F357, $A6327623, $93A83531, $56CCCD02,
  $ACF08162, $5A75EBB5, $6E163697, $88D273CC,
  $DE966292, $81B949D0, $4C50901B, $71C65614,
  $E6C6C7BD, $327A140A, $45E1D006, $C3F27B9A,

  $C9AA53FD, $62A80F00, $BB25BFE2, $35BDD2F6,
  $71126905, $B2040222, $B6CBCF7C, $CD769C2B,
  $53113EC0, $1640E3D3, $38ABBD60, $2547ADF0,
  $BA38209C, $F746CE76, $77AFA1C5, $20756060,

  $85CBFE4E, $8AE88DD8, $7AAAF9B0, $4CF9AA7E,
  $1948C25C, $02FB8A8C, $01C36AE4, $D6EBE1F9,
  $90D4F869, $A65CDEA0, $3F09252D, $C208E69F,
  $B74E6132, $CE77E25B, $578FDFE3, $3AC372E6)
);

{ SHA-1 constants }
const
  { 5 magic numbers }
  cALCipherSHA1_A = DWORD( $67452301 );
  cALCipherSHA1_B = DWORD( $EFCDAB89 );
  cALCipherSHA1_C = DWORD( $98BADCFE );
  cALCipherSHA1_D = DWORD( $10325476 );
  cALCipherSHA1_E = DWORD( $C3D2E1F0 );
  { four rounds consts }
  cALCipherSHA1_K1 = DWORD( $5A827999 );
  cALCipherSHA1_K2 = DWORD( $6ED9EBA1 );
  cALCipherSHA1_K3 = DWORD( $8F1BBCDC );
  cALCipherSHA1_K4 = DWORD( $CA62C1D6 );
  { Maskes used in byte swap }
  cALCipherLBMASK_HI = DWORD( $FF0000 );
  cALCipherLBMASK_LO = DWORD( $FF00 );


{ Rijndael constants }
const
  cALCipherRDLNb128 = 4;      { 128 bit block }
  cALCipherRDLNb192 = 6;      { 192 bit block (not used) }
  cALCipherRDLNb256 = 8;      { 256 bit block (not used) }

  cALCipherRDLNk128 = 4;      { 128 bit key }
  cALCipherRDLNk192 = 6;      { 192 bit key }
  cALCipherRDLNk256 = 8;      { 256 bit key }

{ Rijndael structures }
type
  TALCipherRDLVectors = array[0..(cALCipherRDLNb128 - 1)] of TALCipherRDLVector;
  TALCipherRDLMixColMatrix = array[0..3, 0..3] of Byte;

{-Rijndael lookup tables}

const
  cALCipherRDLSBox : array[$00..$FF] of Byte =
   ($63, $7C, $77, $7B, $F2, $6B, $6F, $C5, $30, $01, $67, $2B, $FE, $D7, $AB, $76,
    $CA, $82, $C9, $7D, $FA, $59, $47, $F0, $AD, $D4, $A2, $AF, $9C, $A4, $72, $C0,
    $B7, $FD, $93, $26, $36, $3F, $F7, $CC, $34, $A5, $E5, $F1, $71, $D8, $31, $15,
    $04, $C7, $23, $C3, $18, $96, $05, $9A, $07, $12, $80, $E2, $EB, $27, $B2, $75,
    $09, $83, $2C, $1A, $1B, $6E, $5A, $A0, $52, $3B, $D6, $B3, $29, $E3, $2F, $84,
    $53, $D1, $00, $ED, $20, $FC, $B1, $5B, $6A, $CB, $BE, $39, $4A, $4C, $58, $CF,
    $D0, $EF, $AA, $FB, $43, $4D, $33, $85, $45, $F9, $02, $7F, $50, $3C, $9F, $A8,
    $51, $A3, $40, $8F, $92, $9D, $38, $F5, $BC, $B6, $DA, $21, $10, $FF, $F3, $D2,
    $CD, $0C, $13, $EC, $5F, $97, $44, $17, $C4, $A7, $7E, $3D, $64, $5D, $19, $73,
    $60, $81, $4F, $DC, $22, $2A, $90, $88, $46, $EE, $B8, $14, $DE, $5E, $0B, $DB,
    $E0, $32, $3A, $0A, $49, $06, $24, $5C, $C2, $D3, $AC, $62, $91, $95, $E4, $79,
    $E7, $C8, $37, $6D, $8D, $D5, $4E, $A9, $6C, $56, $F4, $EA, $65, $7A, $AE, $08,
    $BA, $78, $25, $2E, $1C, $A6, $B4, $C6, $E8, $DD, $74, $1F, $4B, $BD, $8B, $8A,
    $70, $3E, $B5, $66, $48, $03, $F6, $0E, $61, $35, $57, $B9, $86, $C1, $1D, $9E,
    $E1, $F8, $98, $11, $69, $D9, $8E, $94, $9B, $1E, $87, $E9, $CE, $55, $28, $DF,
    $8C, $A1, $89, $0D, $BF, $E6, $42, $68, $41, $99, $2D, $0F, $B0, $54, $BB, $16);

const
  cALCipherRDLInvSBox : array[$00..$FF] of Byte =
   ($52, $09, $6A, $D5, $30, $36, $A5, $38, $BF, $40, $A3, $9E, $81, $F3, $D7, $FB,
    $7C, $E3, $39, $82, $9B, $2F, $FF, $87, $34, $8E, $43, $44, $C4, $DE, $E9, $CB,
    $54, $7B, $94, $32, $A6, $C2, $23, $3D, $EE, $4C, $95, $0B, $42, $FA, $C3, $4E,
    $08, $2E, $A1, $66, $28, $D9, $24, $B2, $76, $5B, $A2, $49, $6D, $8B, $D1, $25,
    $72, $F8, $F6, $64, $86, $68, $98, $16, $D4, $A4, $5C, $CC, $5D, $65, $B6, $92,
    $6C, $70, $48, $50, $FD, $ED, $B9, $DA, $5E, $15, $46, $57, $A7, $8D, $9D, $84,
    $90, $D8, $AB, $00, $8C, $BC, $D3, $0A, $F7, $E4, $58, $05, $B8, $B3, $45, $06,
    $D0, $2C, $1E, $8F, $CA, $3F, $0F, $02, $C1, $AF, $BD, $03, $01, $13, $8A, $6B,
    $3A, $91, $11, $41, $4F, $67, $DC, $EA, $97, $F2, $CF, $CE, $F0, $B4, $E6, $73,
    $96, $AC, $74, $22, $E7, $AD, $35, $85, $E2, $F9, $37, $E8, $1C, $75, $DF, $6E,
    $47, $F1, $1A, $71, $1D, $29, $C5, $89, $6F, $B7, $62, $0E, $AA, $18, $BE, $1B,
    $FC, $56, $3E, $4B, $C6, $D2, $79, $20, $9A, $DB, $C0, $FE, $78, $CD, $5A, $F4,
    $1F, $DD, $A8, $33, $88, $07, $C7, $31, $B1, $12, $10, $59, $27, $80, $EC, $5F,
    $60, $51, $7F, $A9, $19, $B5, $4A, $0D, $2D, $E5, $7A, $9F, $93, $C9, $9C, $EF,
    $A0, $E0, $3B, $4D, $AE, $2A, $F5, $B0, $C8, $EB, $BB, $3C, $83, $53, $99, $61,
    $17, $2B, $04, $7E, $BA, $77, $D6, $26, $E1, $69, $14, $63, $55, $21, $0C, $7D);

const
  cALCipherRCon : array[1..cALCipherMaxRDLRounds] of DWord =
   ($00000001, $00000002, $00000004, $00000008, $00000010, $00000020, $00000040,
    $00000080, $0000001B, $00000036, $0000006C, $000000D8, $000000AB, $0000004D);

const
  cALCipherRDL_T0 : array[Byte] of DWord =
    ($A56363C6, $847C7CF8, $997777EE, $8D7B7BF6, $0DF2F2FF, $BD6B6BD6, $B16F6FDE, $54C5C591,
     $50303060, $03010102, $A96767CE, $7D2B2B56, $19FEFEE7, $62D7D7B5, $E6ABAB4D, $9A7676EC,
     $45CACA8F, $9D82821F, $40C9C989, $877D7DFA, $15FAFAEF, $EB5959B2, $C947478E, $0BF0F0FB,
     $ECADAD41, $67D4D4B3, $FDA2A25F, $EAAFAF45, $BF9C9C23, $F7A4A453, $967272E4, $5BC0C09B,
     $C2B7B775, $1CFDFDE1, $AE93933D, $6A26264C, $5A36366C, $413F3F7E, $02F7F7F5, $4FCCCC83,
     $5C343468, $F4A5A551, $34E5E5D1, $08F1F1F9, $937171E2, $73D8D8AB, $53313162, $3F15152A,
     $0C040408, $52C7C795, $65232346, $5EC3C39D, $28181830, $A1969637, $0F05050A, $B59A9A2F,
     $0907070E, $36121224, $9B80801B, $3DE2E2DF, $26EBEBCD, $6927274E, $CDB2B27F, $9F7575EA,
     $1B090912, $9E83831D, $742C2C58, $2E1A1A34, $2D1B1B36, $B26E6EDC, $EE5A5AB4, $FBA0A05B,
     $F65252A4, $4D3B3B76, $61D6D6B7, $CEB3B37D, $7B292952, $3EE3E3DD, $712F2F5E, $97848413,
     $F55353A6, $68D1D1B9, $00000000, $2CEDEDC1, $60202040, $1FFCFCE3, $C8B1B179, $ED5B5BB6,
     $BE6A6AD4, $46CBCB8D, $D9BEBE67, $4B393972, $DE4A4A94, $D44C4C98, $E85858B0, $4ACFCF85,
     $6BD0D0BB, $2AEFEFC5, $E5AAAA4F, $16FBFBED, $C5434386, $D74D4D9A, $55333366, $94858511,
     $CF45458A, $10F9F9E9, $06020204, $817F7FFE, $F05050A0, $443C3C78, $BA9F9F25, $E3A8A84B,
     $F35151A2, $FEA3A35D, $C0404080, $8A8F8F05, $AD92923F, $BC9D9D21, $48383870, $04F5F5F1,
     $DFBCBC63, $C1B6B677, $75DADAAF, $63212142, $30101020, $1AFFFFE5, $0EF3F3FD, $6DD2D2BF,
     $4CCDCD81, $140C0C18, $35131326, $2FECECC3, $E15F5FBE, $A2979735, $CC444488, $3917172E,
     $57C4C493, $F2A7A755, $827E7EFC, $473D3D7A, $AC6464C8, $E75D5DBA, $2B191932, $957373E6,
     $A06060C0, $98818119, $D14F4F9E, $7FDCDCA3, $66222244, $7E2A2A54, $AB90903B, $8388880B,
     $CA46468C, $29EEEEC7, $D3B8B86B, $3C141428, $79DEDEA7, $E25E5EBC, $1D0B0B16, $76DBDBAD,
     $3BE0E0DB, $56323264, $4E3A3A74, $1E0A0A14, $DB494992, $0A06060C, $6C242448, $E45C5CB8,
     $5DC2C29F, $6ED3D3BD, $EFACAC43, $A66262C4, $A8919139, $A4959531, $37E4E4D3, $8B7979F2,
     $32E7E7D5, $43C8C88B, $5937376E, $B76D6DDA, $8C8D8D01, $64D5D5B1, $D24E4E9C, $E0A9A949,
     $B46C6CD8, $FA5656AC, $07F4F4F3, $25EAEACF, $AF6565CA, $8E7A7AF4, $E9AEAE47, $18080810,
     $D5BABA6F, $887878F0, $6F25254A, $722E2E5C, $241C1C38, $F1A6A657, $C7B4B473, $51C6C697,
     $23E8E8CB, $7CDDDDA1, $9C7474E8, $211F1F3E, $DD4B4B96, $DCBDBD61, $868B8B0D, $858A8A0F,
     $907070E0, $423E3E7C, $C4B5B571, $AA6666CC, $D8484890, $05030306, $01F6F6F7, $120E0E1C,
     $A36161C2, $5F35356A, $F95757AE, $D0B9B969, $91868617, $58C1C199, $271D1D3A, $B99E9E27,
     $38E1E1D9, $13F8F8EB, $B398982B, $33111122, $BB6969D2, $70D9D9A9, $898E8E07, $A7949433,
     $B69B9B2D, $221E1E3C, $92878715, $20E9E9C9, $49CECE87, $FF5555AA, $78282850, $7ADFDFA5,
     $8F8C8C03, $F8A1A159, $80898909, $170D0D1A, $DABFBF65, $31E6E6D7, $C6424284, $B86868D0,
     $C3414182, $B0999929, $772D2D5A, $110F0F1E, $CBB0B07B, $FC5454A8, $D6BBBB6D, $3A16162C);

const
  cALCipherRDL_T1 : array[Byte] of DWord =
    ($6363C6A5, $7C7CF884, $7777EE99, $7B7BF68D, $F2F2FF0D, $6B6BD6BD, $6F6FDEB1, $C5C59154,
     $30306050, $01010203, $6767CEA9, $2B2B567D, $FEFEE719, $D7D7B562, $ABAB4DE6, $7676EC9A,
     $CACA8F45, $82821F9D, $C9C98940, $7D7DFA87, $FAFAEF15, $5959B2EB, $47478EC9, $F0F0FB0B,
     $ADAD41EC, $D4D4B367, $A2A25FFD, $AFAF45EA, $9C9C23BF, $A4A453F7, $7272E496, $C0C09B5B,
     $B7B775C2, $FDFDE11C, $93933DAE, $26264C6A, $36366C5A, $3F3F7E41, $F7F7F502, $CCCC834F,
     $3434685C, $A5A551F4, $E5E5D134, $F1F1F908, $7171E293, $D8D8AB73, $31316253, $15152A3F,
     $0404080C, $C7C79552, $23234665, $C3C39D5E, $18183028, $969637A1, $05050A0F, $9A9A2FB5,
     $07070E09, $12122436, $80801B9B, $E2E2DF3D, $EBEBCD26, $27274E69, $B2B27FCD, $7575EA9F,
     $0909121B, $83831D9E, $2C2C5874, $1A1A342E, $1B1B362D, $6E6EDCB2, $5A5AB4EE, $A0A05BFB,
     $5252A4F6, $3B3B764D, $D6D6B761, $B3B37DCE, $2929527B, $E3E3DD3E, $2F2F5E71, $84841397,
     $5353A6F5, $D1D1B968, $00000000, $EDEDC12C, $20204060, $FCFCE31F, $B1B179C8, $5B5BB6ED,
     $6A6AD4BE, $CBCB8D46, $BEBE67D9, $3939724B, $4A4A94DE, $4C4C98D4, $5858B0E8, $CFCF854A,
     $D0D0BB6B, $EFEFC52A, $AAAA4FE5, $FBFBED16, $434386C5, $4D4D9AD7, $33336655, $85851194,
     $45458ACF, $F9F9E910, $02020406, $7F7FFE81, $5050A0F0, $3C3C7844, $9F9F25BA, $A8A84BE3,
     $5151A2F3, $A3A35DFE, $404080C0, $8F8F058A, $92923FAD, $9D9D21BC, $38387048, $F5F5F104,
     $BCBC63DF, $B6B677C1, $DADAAF75, $21214263, $10102030, $FFFFE51A, $F3F3FD0E, $D2D2BF6D,
     $CDCD814C, $0C0C1814, $13132635, $ECECC32F, $5F5FBEE1, $979735A2, $444488CC, $17172E39,
     $C4C49357, $A7A755F2, $7E7EFC82, $3D3D7A47, $6464C8AC, $5D5DBAE7, $1919322B, $7373E695,
     $6060C0A0, $81811998, $4F4F9ED1, $DCDCA37F, $22224466, $2A2A547E, $90903BAB, $88880B83,
     $46468CCA, $EEEEC729, $B8B86BD3, $1414283C, $DEDEA779, $5E5EBCE2, $0B0B161D, $DBDBAD76,
     $E0E0DB3B, $32326456, $3A3A744E, $0A0A141E, $494992DB, $06060C0A, $2424486C, $5C5CB8E4,
     $C2C29F5D, $D3D3BD6E, $ACAC43EF, $6262C4A6, $919139A8, $959531A4, $E4E4D337, $7979F28B,
     $E7E7D532, $C8C88B43, $37376E59, $6D6DDAB7, $8D8D018C, $D5D5B164, $4E4E9CD2, $A9A949E0,
     $6C6CD8B4, $5656ACFA, $F4F4F307, $EAEACF25, $6565CAAF, $7A7AF48E, $AEAE47E9, $08081018,
     $BABA6FD5, $7878F088, $25254A6F, $2E2E5C72, $1C1C3824, $A6A657F1, $B4B473C7, $C6C69751,
     $E8E8CB23, $DDDDA17C, $7474E89C, $1F1F3E21, $4B4B96DD, $BDBD61DC, $8B8B0D86, $8A8A0F85,
     $7070E090, $3E3E7C42, $B5B571C4, $6666CCAA, $484890D8, $03030605, $F6F6F701, $0E0E1C12,
     $6161C2A3, $35356A5F, $5757AEF9, $B9B969D0, $86861791, $C1C19958, $1D1D3A27, $9E9E27B9,
     $E1E1D938, $F8F8EB13, $98982BB3, $11112233, $6969D2BB, $D9D9A970, $8E8E0789, $949433A7,
     $9B9B2DB6, $1E1E3C22, $87871592, $E9E9C920, $CECE8749, $5555AAFF, $28285078, $DFDFA57A,
     $8C8C038F, $A1A159F8, $89890980, $0D0D1A17, $BFBF65DA, $E6E6D731, $424284C6, $6868D0B8,
     $414182C3, $999929B0, $2D2D5A77, $0F0F1E11, $B0B07BCB, $5454A8FC, $BBBB6DD6, $16162C3A);

const
  cALCipherRDL_T2 : array[Byte] of DWord =
    ($63C6A563, $7CF8847C, $77EE9977, $7BF68D7B, $F2FF0DF2, $6BD6BD6B, $6FDEB16F, $C59154C5,
     $30605030, $01020301, $67CEA967, $2B567D2B, $FEE719FE, $D7B562D7, $AB4DE6AB, $76EC9A76,
     $CA8F45CA, $821F9D82, $C98940C9, $7DFA877D, $FAEF15FA, $59B2EB59, $478EC947, $F0FB0BF0,
     $AD41ECAD, $D4B367D4, $A25FFDA2, $AF45EAAF, $9C23BF9C, $A453F7A4, $72E49672, $C09B5BC0,
     $B775C2B7, $FDE11CFD, $933DAE93, $264C6A26, $366C5A36, $3F7E413F, $F7F502F7, $CC834FCC,
     $34685C34, $A551F4A5, $E5D134E5, $F1F908F1, $71E29371, $D8AB73D8, $31625331, $152A3F15,
     $04080C04, $C79552C7, $23466523, $C39D5EC3, $18302818, $9637A196, $050A0F05, $9A2FB59A,
     $070E0907, $12243612, $801B9B80, $E2DF3DE2, $EBCD26EB, $274E6927, $B27FCDB2, $75EA9F75,
     $09121B09, $831D9E83, $2C58742C, $1A342E1A, $1B362D1B, $6EDCB26E, $5AB4EE5A, $A05BFBA0,
     $52A4F652, $3B764D3B, $D6B761D6, $B37DCEB3, $29527B29, $E3DD3EE3, $2F5E712F, $84139784,
     $53A6F553, $D1B968D1, $00000000, $EDC12CED, $20406020, $FCE31FFC, $B179C8B1, $5BB6ED5B,
     $6AD4BE6A, $CB8D46CB, $BE67D9BE, $39724B39, $4A94DE4A, $4C98D44C, $58B0E858, $CF854ACF,
     $D0BB6BD0, $EFC52AEF, $AA4FE5AA, $FBED16FB, $4386C543, $4D9AD74D, $33665533, $85119485,
     $458ACF45, $F9E910F9, $02040602, $7FFE817F, $50A0F050, $3C78443C, $9F25BA9F, $A84BE3A8,
     $51A2F351, $A35DFEA3, $4080C040, $8F058A8F, $923FAD92, $9D21BC9D, $38704838, $F5F104F5,
     $BC63DFBC, $B677C1B6, $DAAF75DA, $21426321, $10203010, $FFE51AFF, $F3FD0EF3, $D2BF6DD2,
     $CD814CCD, $0C18140C, $13263513, $ECC32FEC, $5FBEE15F, $9735A297, $4488CC44, $172E3917,
     $C49357C4, $A755F2A7, $7EFC827E, $3D7A473D, $64C8AC64, $5DBAE75D, $19322B19, $73E69573,
     $60C0A060, $81199881, $4F9ED14F, $DCA37FDC, $22446622, $2A547E2A, $903BAB90, $880B8388,
     $468CCA46, $EEC729EE, $B86BD3B8, $14283C14, $DEA779DE, $5EBCE25E, $0B161D0B, $DBAD76DB,
     $E0DB3BE0, $32645632, $3A744E3A, $0A141E0A, $4992DB49, $060C0A06, $24486C24, $5CB8E45C,
     $C29F5DC2, $D3BD6ED3, $AC43EFAC, $62C4A662, $9139A891, $9531A495, $E4D337E4, $79F28B79,
     $E7D532E7, $C88B43C8, $376E5937, $6DDAB76D, $8D018C8D, $D5B164D5, $4E9CD24E, $A949E0A9,
     $6CD8B46C, $56ACFA56, $F4F307F4, $EACF25EA, $65CAAF65, $7AF48E7A, $AE47E9AE, $08101808,
     $BA6FD5BA, $78F08878, $254A6F25, $2E5C722E, $1C38241C, $A657F1A6, $B473C7B4, $C69751C6,
     $E8CB23E8, $DDA17CDD, $74E89C74, $1F3E211F, $4B96DD4B, $BD61DCBD, $8B0D868B, $8A0F858A,
     $70E09070, $3E7C423E, $B571C4B5, $66CCAA66, $4890D848, $03060503, $F6F701F6, $0E1C120E,
     $61C2A361, $356A5F35, $57AEF957, $B969D0B9, $86179186, $C19958C1, $1D3A271D, $9E27B99E,
     $E1D938E1, $F8EB13F8, $982BB398, $11223311, $69D2BB69, $D9A970D9, $8E07898E, $9433A794,
     $9B2DB69B, $1E3C221E, $87159287, $E9C920E9, $CE8749CE, $55AAFF55, $28507828, $DFA57ADF,
     $8C038F8C, $A159F8A1, $89098089, $0D1A170D, $BF65DABF, $E6D731E6, $4284C642, $68D0B868,
     $4182C341, $9929B099, $2D5A772D, $0F1E110F, $B07BCBB0, $54A8FC54, $BB6DD6BB, $162C3A16);

const
  cALCipherRDL_T3 : array[Byte] of DWord =
    ($C6A56363, $F8847C7C, $EE997777, $F68D7B7B, $FF0DF2F2, $D6BD6B6B, $DEB16F6F, $9154C5C5,
     $60503030, $02030101, $CEA96767, $567D2B2B, $E719FEFE, $B562D7D7, $4DE6ABAB, $EC9A7676,
     $8F45CACA, $1F9D8282, $8940C9C9, $FA877D7D, $EF15FAFA, $B2EB5959, $8EC94747, $FB0BF0F0,
     $41ECADAD, $B367D4D4, $5FFDA2A2, $45EAAFAF, $23BF9C9C, $53F7A4A4, $E4967272, $9B5BC0C0,
     $75C2B7B7, $E11CFDFD, $3DAE9393, $4C6A2626, $6C5A3636, $7E413F3F, $F502F7F7, $834FCCCC,
     $685C3434, $51F4A5A5, $D134E5E5, $F908F1F1, $E2937171, $AB73D8D8, $62533131, $2A3F1515,
     $080C0404, $9552C7C7, $46652323, $9D5EC3C3, $30281818, $37A19696, $0A0F0505, $2FB59A9A,
     $0E090707, $24361212, $1B9B8080, $DF3DE2E2, $CD26EBEB, $4E692727, $7FCDB2B2, $EA9F7575,
     $121B0909, $1D9E8383, $58742C2C, $342E1A1A, $362D1B1B, $DCB26E6E, $B4EE5A5A, $5BFBA0A0,
     $A4F65252, $764D3B3B, $B761D6D6, $7DCEB3B3, $527B2929, $DD3EE3E3, $5E712F2F, $13978484,
     $A6F55353, $B968D1D1, $00000000, $C12CEDED, $40602020, $E31FFCFC, $79C8B1B1, $B6ED5B5B,
     $D4BE6A6A, $8D46CBCB, $67D9BEBE, $724B3939, $94DE4A4A, $98D44C4C, $B0E85858, $854ACFCF,
     $BB6BD0D0, $C52AEFEF, $4FE5AAAA, $ED16FBFB, $86C54343, $9AD74D4D, $66553333, $11948585,
     $8ACF4545, $E910F9F9, $04060202, $FE817F7F, $A0F05050, $78443C3C, $25BA9F9F, $4BE3A8A8,
     $A2F35151, $5DFEA3A3, $80C04040, $058A8F8F, $3FAD9292, $21BC9D9D, $70483838, $F104F5F5,
     $63DFBCBC, $77C1B6B6, $AF75DADA, $42632121, $20301010, $E51AFFFF, $FD0EF3F3, $BF6DD2D2,
     $814CCDCD, $18140C0C, $26351313, $C32FECEC, $BEE15F5F, $35A29797, $88CC4444, $2E391717,
     $9357C4C4, $55F2A7A7, $FC827E7E, $7A473D3D, $C8AC6464, $BAE75D5D, $322B1919, $E6957373,
     $C0A06060, $19988181, $9ED14F4F, $A37FDCDC, $44662222, $547E2A2A, $3BAB9090, $0B838888,
     $8CCA4646, $C729EEEE, $6BD3B8B8, $283C1414, $A779DEDE, $BCE25E5E, $161D0B0B, $AD76DBDB,
     $DB3BE0E0, $64563232, $744E3A3A, $141E0A0A, $92DB4949, $0C0A0606, $486C2424, $B8E45C5C,
     $9F5DC2C2, $BD6ED3D3, $43EFACAC, $C4A66262, $39A89191, $31A49595, $D337E4E4, $F28B7979,
     $D532E7E7, $8B43C8C8, $6E593737, $DAB76D6D, $018C8D8D, $B164D5D5, $9CD24E4E, $49E0A9A9,
     $D8B46C6C, $ACFA5656, $F307F4F4, $CF25EAEA, $CAAF6565, $F48E7A7A, $47E9AEAE, $10180808,
     $6FD5BABA, $F0887878, $4A6F2525, $5C722E2E, $38241C1C, $57F1A6A6, $73C7B4B4, $9751C6C6,
     $CB23E8E8, $A17CDDDD, $E89C7474, $3E211F1F, $96DD4B4B, $61DCBDBD, $0D868B8B, $0F858A8A,
     $E0907070, $7C423E3E, $71C4B5B5, $CCAA6666, $90D84848, $06050303, $F701F6F6, $1C120E0E,
     $C2A36161, $6A5F3535, $AEF95757, $69D0B9B9, $17918686, $9958C1C1, $3A271D1D, $27B99E9E,
     $D938E1E1, $EB13F8F8, $2BB39898, $22331111, $D2BB6969, $A970D9D9, $07898E8E, $33A79494,
     $2DB69B9B, $3C221E1E, $15928787, $C920E9E9, $8749CECE, $AAFF5555, $50782828, $A57ADFDF,
     $038F8C8C, $59F8A1A1, $09808989, $1A170D0D, $65DABFBF, $D731E6E6, $84C64242, $D0B86868,
     $82C34141, $29B09999, $5A772D2D, $1E110F0F, $7BCBB0B0, $A8FC5454, $6DD6BBBB, $2C3A1616);

const
  cALCipherRDL_InvT0 : array[Byte] of DWord =
    ($00000000, $0B0D090E, $161A121C, $1D171B12, $2C342438, $27392D36, $3A2E3624, $31233F2A,
     $58684870, $5365417E, $4E725A6C, $457F5362, $745C6C48, $7F516546, $62467E54, $694B775A,
     $B0D090E0, $BBDD99EE, $A6CA82FC, $ADC78BF2, $9CE4B4D8, $97E9BDD6, $8AFEA6C4, $81F3AFCA,
     $E8B8D890, $E3B5D19E, $FEA2CA8C, $F5AFC382, $C48CFCA8, $CF81F5A6, $D296EEB4, $D99BE7BA,
     $7BBB3BDB, $70B632D5, $6DA129C7, $66AC20C9, $578F1FE3, $5C8216ED, $41950DFF, $4A9804F1,
     $23D373AB, $28DE7AA5, $35C961B7, $3EC468B9, $0FE75793, $04EA5E9D, $19FD458F, $12F04C81,
     $CB6BAB3B, $C066A235, $DD71B927, $D67CB029, $E75F8F03, $EC52860D, $F1459D1F, $FA489411,
     $9303E34B, $980EEA45, $8519F157, $8E14F859, $BF37C773, $B43ACE7D, $A92DD56F, $A220DC61,
     $F66D76AD, $FD607FA3, $E07764B1, $EB7A6DBF, $DA595295, $D1545B9B, $CC434089, $C74E4987,
     $AE053EDD, $A50837D3, $B81F2CC1, $B31225CF, $82311AE5, $893C13EB, $942B08F9, $9F2601F7,
     $46BDE64D, $4DB0EF43, $50A7F451, $5BAAFD5F, $6A89C275, $6184CB7B, $7C93D069, $779ED967,
     $1ED5AE3D, $15D8A733, $08CFBC21, $03C2B52F, $32E18A05, $39EC830B, $24FB9819, $2FF69117,
     $8DD64D76, $86DB4478, $9BCC5F6A, $90C15664, $A1E2694E, $AAEF6040, $B7F87B52, $BCF5725C,
     $D5BE0506, $DEB30C08, $C3A4171A, $C8A91E14, $F98A213E, $F2872830, $EF903322, $E49D3A2C,
     $3D06DD96, $360BD498, $2B1CCF8A, $2011C684, $1132F9AE, $1A3FF0A0, $0728EBB2, $0C25E2BC,
     $656E95E6, $6E639CE8, $737487FA, $78798EF4, $495AB1DE, $4257B8D0, $5F40A3C2, $544DAACC,
     $F7DAEC41, $FCD7E54F, $E1C0FE5D, $EACDF753, $DBEEC879, $D0E3C177, $CDF4DA65, $C6F9D36B,
     $AFB2A431, $A4BFAD3F, $B9A8B62D, $B2A5BF23, $83868009, $888B8907, $959C9215, $9E919B1B,
     $470A7CA1, $4C0775AF, $51106EBD, $5A1D67B3, $6B3E5899, $60335197, $7D244A85, $7629438B,
     $1F6234D1, $146F3DDF, $097826CD, $02752FC3, $335610E9, $385B19E7, $254C02F5, $2E410BFB,
     $8C61D79A, $876CDE94, $9A7BC586, $9176CC88, $A055F3A2, $AB58FAAC, $B64FE1BE, $BD42E8B0,
     $D4099FEA, $DF0496E4, $C2138DF6, $C91E84F8, $F83DBBD2, $F330B2DC, $EE27A9CE, $E52AA0C0,
     $3CB1477A, $37BC4E74, $2AAB5566, $21A65C68, $10856342, $1B886A4C, $069F715E, $0D927850,
     $64D90F0A, $6FD40604, $72C31D16, $79CE1418, $48ED2B32, $43E0223C, $5EF7392E, $55FA3020,
     $01B79AEC, $0ABA93E2, $17AD88F0, $1CA081FE, $2D83BED4, $268EB7DA, $3B99ACC8, $3094A5C6,
     $59DFD29C, $52D2DB92, $4FC5C080, $44C8C98E, $75EBF6A4, $7EE6FFAA, $63F1E4B8, $68FCEDB6,
     $B1670A0C, $BA6A0302, $A77D1810, $AC70111E, $9D532E34, $965E273A, $8B493C28, $80443526,
     $E90F427C, $E2024B72, $FF155060, $F418596E, $C53B6644, $CE366F4A, $D3217458, $D82C7D56,
     $7A0CA137, $7101A839, $6C16B32B, $671BBA25, $5638850F, $5D358C01, $40229713, $4B2F9E1D,
     $2264E947, $2969E049, $347EFB5B, $3F73F255, $0E50CD7F, $055DC471, $184ADF63, $1347D66D,
     $CADC31D7, $C1D138D9, $DCC623CB, $D7CB2AC5, $E6E815EF, $EDE51CE1, $F0F207F3, $FBFF0EFD,
     $92B479A7, $99B970A9, $84AE6BBB, $8FA362B5, $BE805D9F, $B58D5491, $A89A4F83, $A397468D);

const
  cALCipherRDL_InvT1 : array[Byte] of DWord =
    ($00000000, $0D090E0B, $1A121C16, $171B121D, $3424382C, $392D3627, $2E36243A, $233F2A31,
     $68487058, $65417E53, $725A6C4E, $7F536245, $5C6C4874, $5165467F, $467E5462, $4B775A69,
     $D090E0B0, $DD99EEBB, $CA82FCA6, $C78BF2AD, $E4B4D89C, $E9BDD697, $FEA6C48A, $F3AFCA81,
     $B8D890E8, $B5D19EE3, $A2CA8CFE, $AFC382F5, $8CFCA8C4, $81F5A6CF, $96EEB4D2, $9BE7BAD9,
     $BB3BDB7B, $B632D570, $A129C76D, $AC20C966, $8F1FE357, $8216ED5C, $950DFF41, $9804F14A,
     $D373AB23, $DE7AA528, $C961B735, $C468B93E, $E757930F, $EA5E9D04, $FD458F19, $F04C8112,
     $6BAB3BCB, $66A235C0, $71B927DD, $7CB029D6, $5F8F03E7, $52860DEC, $459D1FF1, $489411FA,
     $03E34B93, $0EEA4598, $19F15785, $14F8598E, $37C773BF, $3ACE7DB4, $2DD56FA9, $20DC61A2,
     $6D76ADF6, $607FA3FD, $7764B1E0, $7A6DBFEB, $595295DA, $545B9BD1, $434089CC, $4E4987C7,
     $053EDDAE, $0837D3A5, $1F2CC1B8, $1225CFB3, $311AE582, $3C13EB89, $2B08F994, $2601F79F,
     $BDE64D46, $B0EF434D, $A7F45150, $AAFD5F5B, $89C2756A, $84CB7B61, $93D0697C, $9ED96777,
     $D5AE3D1E, $D8A73315, $CFBC2108, $C2B52F03, $E18A0532, $EC830B39, $FB981924, $F691172F,
     $D64D768D, $DB447886, $CC5F6A9B, $C1566490, $E2694EA1, $EF6040AA, $F87B52B7, $F5725CBC,
     $BE0506D5, $B30C08DE, $A4171AC3, $A91E14C8, $8A213EF9, $872830F2, $903322EF, $9D3A2CE4,
     $06DD963D, $0BD49836, $1CCF8A2B, $11C68420, $32F9AE11, $3FF0A01A, $28EBB207, $25E2BC0C,
     $6E95E665, $639CE86E, $7487FA73, $798EF478, $5AB1DE49, $57B8D042, $40A3C25F, $4DAACC54,
     $DAEC41F7, $D7E54FFC, $C0FE5DE1, $CDF753EA, $EEC879DB, $E3C177D0, $F4DA65CD, $F9D36BC6,
     $B2A431AF, $BFAD3FA4, $A8B62DB9, $A5BF23B2, $86800983, $8B890788, $9C921595, $919B1B9E,
     $0A7CA147, $0775AF4C, $106EBD51, $1D67B35A, $3E58996B, $33519760, $244A857D, $29438B76,
     $6234D11F, $6F3DDF14, $7826CD09, $752FC302, $5610E933, $5B19E738, $4C02F525, $410BFB2E,
     $61D79A8C, $6CDE9487, $7BC5869A, $76CC8891, $55F3A2A0, $58FAACAB, $4FE1BEB6, $42E8B0BD,
     $099FEAD4, $0496E4DF, $138DF6C2, $1E84F8C9, $3DBBD2F8, $30B2DCF3, $27A9CEEE, $2AA0C0E5,
     $B1477A3C, $BC4E7437, $AB55662A, $A65C6821, $85634210, $886A4C1B, $9F715E06, $9278500D,
     $D90F0A64, $D406046F, $C31D1672, $CE141879, $ED2B3248, $E0223C43, $F7392E5E, $FA302055,
     $B79AEC01, $BA93E20A, $AD88F017, $A081FE1C, $83BED42D, $8EB7DA26, $99ACC83B, $94A5C630,
     $DFD29C59, $D2DB9252, $C5C0804F, $C8C98E44, $EBF6A475, $E6FFAA7E, $F1E4B863, $FCEDB668,
     $670A0CB1, $6A0302BA, $7D1810A7, $70111EAC, $532E349D, $5E273A96, $493C288B, $44352680,
     $0F427CE9, $024B72E2, $155060FF, $18596EF4, $3B6644C5, $366F4ACE, $217458D3, $2C7D56D8,
     $0CA1377A, $01A83971, $16B32B6C, $1BBA2567, $38850F56, $358C015D, $22971340, $2F9E1D4B,
     $64E94722, $69E04929, $7EFB5B34, $73F2553F, $50CD7F0E, $5DC47105, $4ADF6318, $47D66D13,
     $DC31D7CA, $D138D9C1, $C623CBDC, $CB2AC5D7, $E815EFE6, $E51CE1ED, $F207F3F0, $FF0EFDFB,
     $B479A792, $B970A999, $AE6BBB84, $A362B58F, $805D9FBE, $8D5491B5, $9A4F83A8, $97468DA3);

const
  cALCipherRDL_InvT2 : array[Byte] of DWord =
    ($00000000, $090E0B0D, $121C161A, $1B121D17, $24382C34, $2D362739, $36243A2E, $3F2A3123,
     $48705868, $417E5365, $5A6C4E72, $5362457F, $6C48745C, $65467F51, $7E546246, $775A694B,
     $90E0B0D0, $99EEBBDD, $82FCA6CA, $8BF2ADC7, $B4D89CE4, $BDD697E9, $A6C48AFE, $AFCA81F3,
     $D890E8B8, $D19EE3B5, $CA8CFEA2, $C382F5AF, $FCA8C48C, $F5A6CF81, $EEB4D296, $E7BAD99B,
     $3BDB7BBB, $32D570B6, $29C76DA1, $20C966AC, $1FE3578F, $16ED5C82, $0DFF4195, $04F14A98,
     $73AB23D3, $7AA528DE, $61B735C9, $68B93EC4, $57930FE7, $5E9D04EA, $458F19FD, $4C8112F0,
     $AB3BCB6B, $A235C066, $B927DD71, $B029D67C, $8F03E75F, $860DEC52, $9D1FF145, $9411FA48,
     $E34B9303, $EA45980E, $F1578519, $F8598E14, $C773BF37, $CE7DB43A, $D56FA92D, $DC61A220,
     $76ADF66D, $7FA3FD60, $64B1E077, $6DBFEB7A, $5295DA59, $5B9BD154, $4089CC43, $4987C74E,
     $3EDDAE05, $37D3A508, $2CC1B81F, $25CFB312, $1AE58231, $13EB893C, $08F9942B, $01F79F26,
     $E64D46BD, $EF434DB0, $F45150A7, $FD5F5BAA, $C2756A89, $CB7B6184, $D0697C93, $D967779E,
     $AE3D1ED5, $A73315D8, $BC2108CF, $B52F03C2, $8A0532E1, $830B39EC, $981924FB, $91172FF6,
     $4D768DD6, $447886DB, $5F6A9BCC, $566490C1, $694EA1E2, $6040AAEF, $7B52B7F8, $725CBCF5,
     $0506D5BE, $0C08DEB3, $171AC3A4, $1E14C8A9, $213EF98A, $2830F287, $3322EF90, $3A2CE49D,
     $DD963D06, $D498360B, $CF8A2B1C, $C6842011, $F9AE1132, $F0A01A3F, $EBB20728, $E2BC0C25,
     $95E6656E, $9CE86E63, $87FA7374, $8EF47879, $B1DE495A, $B8D04257, $A3C25F40, $AACC544D,
     $EC41F7DA, $E54FFCD7, $FE5DE1C0, $F753EACD, $C879DBEE, $C177D0E3, $DA65CDF4, $D36BC6F9,
     $A431AFB2, $AD3FA4BF, $B62DB9A8, $BF23B2A5, $80098386, $8907888B, $9215959C, $9B1B9E91,
     $7CA1470A, $75AF4C07, $6EBD5110, $67B35A1D, $58996B3E, $51976033, $4A857D24, $438B7629,
     $34D11F62, $3DDF146F, $26CD0978, $2FC30275, $10E93356, $19E7385B, $02F5254C, $0BFB2E41,
     $D79A8C61, $DE94876C, $C5869A7B, $CC889176, $F3A2A055, $FAACAB58, $E1BEB64F, $E8B0BD42,
     $9FEAD409, $96E4DF04, $8DF6C213, $84F8C91E, $BBD2F83D, $B2DCF330, $A9CEEE27, $A0C0E52A,
     $477A3CB1, $4E7437BC, $55662AAB, $5C6821A6, $63421085, $6A4C1B88, $715E069F, $78500D92,
     $0F0A64D9, $06046FD4, $1D1672C3, $141879CE, $2B3248ED, $223C43E0, $392E5EF7, $302055FA,
     $9AEC01B7, $93E20ABA, $88F017AD, $81FE1CA0, $BED42D83, $B7DA268E, $ACC83B99, $A5C63094,
     $D29C59DF, $DB9252D2, $C0804FC5, $C98E44C8, $F6A475EB, $FFAA7EE6, $E4B863F1, $EDB668FC,
     $0A0CB167, $0302BA6A, $1810A77D, $111EAC70, $2E349D53, $273A965E, $3C288B49, $35268044,
     $427CE90F, $4B72E202, $5060FF15, $596EF418, $6644C53B, $6F4ACE36, $7458D321, $7D56D82C,
     $A1377A0C, $A8397101, $B32B6C16, $BA25671B, $850F5638, $8C015D35, $97134022, $9E1D4B2F,
     $E9472264, $E0492969, $FB5B347E, $F2553F73, $CD7F0E50, $C471055D, $DF63184A, $D66D1347,
     $31D7CADC, $38D9C1D1, $23CBDCC6, $2AC5D7CB, $15EFE6E8, $1CE1EDE5, $07F3F0F2, $0EFDFBFF,
     $79A792B4, $70A999B9, $6BBB84AE, $62B58FA3, $5D9FBE80, $5491B58D, $4F83A89A, $468DA397);

const
  cALCipherRDL_InvT3 : array[Byte] of DWord =
    ($00000000, $0E0B0D09, $1C161A12, $121D171B, $382C3424, $3627392D, $243A2E36, $2A31233F,
     $70586848, $7E536541, $6C4E725A, $62457F53, $48745C6C, $467F5165, $5462467E, $5A694B77,
     $E0B0D090, $EEBBDD99, $FCA6CA82, $F2ADC78B, $D89CE4B4, $D697E9BD, $C48AFEA6, $CA81F3AF,
     $90E8B8D8, $9EE3B5D1, $8CFEA2CA, $82F5AFC3, $A8C48CFC, $A6CF81F5, $B4D296EE, $BAD99BE7,
     $DB7BBB3B, $D570B632, $C76DA129, $C966AC20, $E3578F1F, $ED5C8216, $FF41950D, $F14A9804,
     $AB23D373, $A528DE7A, $B735C961, $B93EC468, $930FE757, $9D04EA5E, $8F19FD45, $8112F04C,
     $3BCB6BAB, $35C066A2, $27DD71B9, $29D67CB0, $03E75F8F, $0DEC5286, $1FF1459D, $11FA4894,
     $4B9303E3, $45980EEA, $578519F1, $598E14F8, $73BF37C7, $7DB43ACE, $6FA92DD5, $61A220DC,
     $ADF66D76, $A3FD607F, $B1E07764, $BFEB7A6D, $95DA5952, $9BD1545B, $89CC4340, $87C74E49,
     $DDAE053E, $D3A50837, $C1B81F2C, $CFB31225, $E582311A, $EB893C13, $F9942B08, $F79F2601,
     $4D46BDE6, $434DB0EF, $5150A7F4, $5F5BAAFD, $756A89C2, $7B6184CB, $697C93D0, $67779ED9,
     $3D1ED5AE, $3315D8A7, $2108CFBC, $2F03C2B5, $0532E18A, $0B39EC83, $1924FB98, $172FF691,
     $768DD64D, $7886DB44, $6A9BCC5F, $6490C156, $4EA1E269, $40AAEF60, $52B7F87B, $5CBCF572,
     $06D5BE05, $08DEB30C, $1AC3A417, $14C8A91E, $3EF98A21, $30F28728, $22EF9033, $2CE49D3A,
     $963D06DD, $98360BD4, $8A2B1CCF, $842011C6, $AE1132F9, $A01A3FF0, $B20728EB, $BC0C25E2,
     $E6656E95, $E86E639C, $FA737487, $F478798E, $DE495AB1, $D04257B8, $C25F40A3, $CC544DAA,
     $41F7DAEC, $4FFCD7E5, $5DE1C0FE, $53EACDF7, $79DBEEC8, $77D0E3C1, $65CDF4DA, $6BC6F9D3,
     $31AFB2A4, $3FA4BFAD, $2DB9A8B6, $23B2A5BF, $09838680, $07888B89, $15959C92, $1B9E919B,
     $A1470A7C, $AF4C0775, $BD51106E, $B35A1D67, $996B3E58, $97603351, $857D244A, $8B762943,
     $D11F6234, $DF146F3D, $CD097826, $C302752F, $E9335610, $E7385B19, $F5254C02, $FB2E410B,
     $9A8C61D7, $94876CDE, $869A7BC5, $889176CC, $A2A055F3, $ACAB58FA, $BEB64FE1, $B0BD42E8,
     $EAD4099F, $E4DF0496, $F6C2138D, $F8C91E84, $D2F83DBB, $DCF330B2, $CEEE27A9, $C0E52AA0,
     $7A3CB147, $7437BC4E, $662AAB55, $6821A65C, $42108563, $4C1B886A, $5E069F71, $500D9278,
     $0A64D90F, $046FD406, $1672C31D, $1879CE14, $3248ED2B, $3C43E022, $2E5EF739, $2055FA30,
     $EC01B79A, $E20ABA93, $F017AD88, $FE1CA081, $D42D83BE, $DA268EB7, $C83B99AC, $C63094A5,
     $9C59DFD2, $9252D2DB, $804FC5C0, $8E44C8C9, $A475EBF6, $AA7EE6FF, $B863F1E4, $B668FCED,
     $0CB1670A, $02BA6A03, $10A77D18, $1EAC7011, $349D532E, $3A965E27, $288B493C, $26804435,
     $7CE90F42, $72E2024B, $60FF1550, $6EF41859, $44C53B66, $4ACE366F, $58D32174, $56D82C7D,
     $377A0CA1, $397101A8, $2B6C16B3, $25671BBA, $0F563885, $015D358C, $13402297, $1D4B2F9E,
     $472264E9, $492969E0, $5B347EFB, $553F73F2, $7F0E50CD, $71055DC4, $63184ADF, $6D1347D6,
     $D7CADC31, $D9C1D138, $CBDCC623, $C5D7CB2A, $EFE6E815, $E1EDE51C, $F3F0F207, $FDFBFF0E,
     $A792B479, $A999B970, $BB84AE6B, $B58FA362, $9FBE805D, $91B58D54, $83A89A4F, $8DA39746);

{***************************************************************************************}
procedure ALCipherInitEncryptBF(Key : TALCipherKey128; var Context : TALCipherBFContext);
var
  I     : Integer;
  J     : Integer;
  K     : Integer;
  Data  : LongInt;
  Block : TALCipherBFBlock;
begin
  {initialize PArray}
  ALMove(cALCipherbf_P, Context.PBox, SizeOf(Context.PBox));
  {initialize SBox}
  ALMove(cALCipherbf_S, Context.SBox, SizeOf(Context.SBox));

  {update PArray with the key bits}
  J := 0;
  for I := 0 to (cALCipherBFRounds+1) do begin
    Data := 0;
    for K := 0 to 3 do begin
      Data := (Data shl 8) or Key[J];
      Inc(J);
      if J >= SizeOf(Key) then
        J := 0;
    end;
    Context.PBox[I] := Context.PBox[I] xor Data;
  end;

  {encrypt an all-zero string using the Blowfish algorithm and}
  {replace the elements of the P-array with the output of this process}

  Block[0] := 0;
  Block[1] := 0;
  I := 0;
  repeat
    ALCipherEncryptBF(Context, Block, True);
    Context.PBox[I] := Block[0];
    Context.PBox[I+1] := Block[1];
    Inc(I, 2);
  until I > cALCipherBFRounds+1;

  {continue the process, replacing the elements of the four S-boxes in}
  {order, with the output of the continuously changing Blowfish algorithm}

  for J := 0 to 3 do begin
    I := 0;
    repeat
      ALCipherEncryptBF(Context, Block, True);
      Context.SBox[J, I] := Block[0];
      Context.SBox[J, I+1] := Block[1];
      Inc(I, 2);
    until I > 255;
  end;

  {in total, 521 iterations are required to generate all required subkeys. }
end;

{***************************************************************************************************************}
procedure ALCipherEncryptBF(const Context : TALCipherBFContext; var Block : TALCipherBFBlock; Encrypt : Boolean);
var
  I : Integer;
  TmpBlock : TALCipherBFBlockEx;
begin
  ALMove(Block, TmpBlock, SizeOf(TmpBlock));
  if Encrypt then begin
    Block[0] := Block[0] xor Context.PBox[0];

    {16 Rounds to go (8 double rounds to avoid swaps)}
    I := 1;
    repeat
      {first half round }
      Block[1] := Block[1] xor Context.PBox[I] xor (((
                  Context.SBox[0, TmpBlock.Xl[3]] + Context.SBox[1, TmpBlock.Xl[2]])
                  xor Context.SBox[2, TmpBlock.Xl[1]]) + Context.SBox[3, TmpBlock.Xl[0]]);
      {second half round }
      Block[0] := Block[0] xor Context.PBox[I+1] xor (((
                  Context.SBox[0, TmpBlock.Xr[3]] + Context.SBox[1, TmpBlock.Xr[2]])
                  xor Context.SBox[2, TmpBlock.Xr[1]]) + Context.SBox[3, TmpBlock.Xr[0]]);
      Inc(I, 2);
    until I > cALCipherBFRounds;
    Block[1] := Block[1] xor Context.PBox[(cALCipherBFRounds+1)];
  end else begin
    Block[1] := Block[1] xor Context.PBox[(cALCipherBFRounds+1)];

    {16 Rounds to go (8 double rounds to avoid swaps)}
    I := cALCipherBFRounds;
    repeat
      {first half round }
      Block[0] := Block[0] xor Context.PBox[I] xor (((
                  Context.SBox[0, TmpBlock.Xr[3]] + Context.SBox[1, TmpBlock.Xr[2]])
                  xor Context.SBox[2, TmpBlock.Xr[1]]) + Context.SBox[3, TmpBlock.Xr[0]]);
      {second half round }
      Block[1] := Block[1] xor Context.PBox[i-1] xor (((
                  Context.SBox[0, TmpBlock.Xl[3]] + Context.SBox[1, TmpBlock.Xl[2]])
                  xor Context.SBox[2, TmpBlock.Xl[1]]) + Context.SBox[3, TmpBlock.Xl[0]]);
       Dec (I, 2);
     until I < 1;
     Block[0] := Block[0] xor Context.PBox[0];
  end;
end;

{***********************************************************************************************}
procedure ALCipherEncryptBFCBC(const Context : TALCipherBFContext; const Prev : TALCipherBFBlock;
  var Block : TALCipherBFBlock; Encrypt : Boolean);
begin
  if Encrypt then begin
    ALCipherXorMem(Block, Prev, SizeOf(Block));
    ALCipherEncryptBF(Context, Block, Encrypt);
  end else begin
    ALCipherEncryptBF(Context, Block, Encrypt);
    ALCipherXorMem(Block, Prev, SizeOf(Block));
  end;
end;

{**************************************************************}
procedure ALCipherGenerateRandomKey(var Key; KeySize : Integer);
var
  I     : Integer;
begin
  Randomize;
  for I := 0 to KeySize - 1 do
    TByteArray(Key)[I] := System.Random(256);
end;

{**********************************************************************************}
procedure ALCipherGenerateMD5Key(var Key : TALCipherKey128; const Str : AnsiString);
var
  D : TALCipherMD5Digest;
begin
  ALCipherHashMD5(D, Str[1], Length(Str));
  Key := TALCipherKey128(D);
end;

{************************************************}
procedure ALCipherMix128(var X : TALCipher128Bit);
var
  AA, BB, CC, DD : LongInt;
begin
  AA := X[0];  BB := X[1];  CC := X[2];  DD := X[3];

  AA := AA + DD;  DD := DD + AA;  AA := AA xor (AA shr 7);
  BB := BB + AA;  AA := AA + BB;  BB := BB xor (BB shl 13);
  CC := CC + BB;  BB := BB + CC;  CC := CC xor (CC shr 17);
  DD := DD + CC;  CC := CC + DD;  DD := DD xor (DD shl 9);
  AA := AA + DD;  DD := DD + AA;  AA := AA xor (AA shr 3);
  BB := BB + AA;  AA := AA + BB;  BB := BB xor (BB shl 7);
  CC := CC + BB;  BB := BB + CC;  CC := CC xor (DD shr 15);
  DD := DD + CC;  CC := CC + DD;  DD := DD xor (DD shl 11);

  X[0] := AA;  X[1] := BB;  X[2] := CC;  X[3] := DD;
end;

{*****************************************************************************}
procedure ALCipherHashELF(var Digest : LongInt; const Buf;  BufSize : LongInt);
var
  I, X  : LongInt;
begin
  Digest := 0;
  for I := 0 to BufSize - 1 do begin
    Digest := (Digest shl 4) + TByteArray(Buf)[I];
    X := Digest and $F0000000;
    if (X <> 0) then
      Digest := Digest xor (X shr 24);
    Digest := Digest and (not X);
  end;
end;

{****************************************************}
function ALCipherRolX(I, C : DWord) : DWord; register;
asm
  mov  ecx, edx         {get count to cl}
  rol  eax, cl          {rotate eax by cl}
end;

{**************************************************************************************}
procedure ALCipherTransform(var Buffer : array of DWord;  const InBuf : array of DWord);
const
  S11 = 7;
  S12 = 12;
  S13 = 17;
  S14 = 22;
  S21 = 5;
  S22 = 9;
  S23 = 14;
  S24 = 20;
  S31 = 4;
  S32 = 11;
  S33 = 16;
  S34 = 23;
  S41 = 6;
  S42 = 10;
  S43 = 15;
  S44 = 21;
var
  Buf : array [0..3] of DWord;                                       
  InA : array [0..15] of DWord;                                      
var
  A   : DWord;
  B   : DWord;
  C   : DWord;
  D   : DWord;

  procedure FF(var A : DWord;  B, C, D, X, S, AC : DWord);
  begin
    A := ALCipherRolX(A + ((B and C) or (not B and D)) + X + AC, S) + B;
  end;

  procedure GG(var A : DWord;  B, C, D, X, S, AC : DWord);
  begin
    A := ALCipherRolX(A + ((B and D) or (C and not D)) + X + AC, S) + B;
  end;

  procedure HH(var A : DWord;  B, C, D, X, S, AC : DWord);
  begin
    A := ALCipherRolX(A + (B xor C xor D) + X + AC, S) + B;
  end;

  procedure II(var A : DWord;  B, C, D, X, S, AC : DWord);
  begin
    A := ALCipherRolX(A + (C xor (B or not D)) + X + AC, S) + B;
  end;

begin
  ALMove(Buffer, Buf, SizeOf(Buf));                                    
  ALMove(InBuf, InA, SizeOf(InA));                                     
  A := Buf [0];
  B := Buf [1];
  C := Buf [2];
  D := Buf [3];


  {round 1}
  FF(A, B, C, D, InA [ 0], S11, $D76AA478);  { 1 }
  FF(D, A, B, C, InA [ 1], S12, $E8C7B756);  { 2 }
  FF(C, D, A, B, InA [ 2], S13, $242070DB);  { 3 }
  FF(B, C, D, A, InA [ 3], S14, $C1BDCEEE);  { 4 }
  FF(A, B, C, D, InA [ 4], S11, $F57C0FAF);  { 5 }
  FF(D, A, B, C, InA [ 5], S12, $4787C62A);  { 6 }
  FF(C, D, A, B, InA [ 6], S13, $A8304613);  { 7 }
  FF(B, C, D, A, InA [ 7], S14, $FD469501);  { 8 }
  FF(A, B, C, D, InA [ 8], S11, $698098D8);  { 9 }
  FF(D, A, B, C, InA [ 9], S12, $8B44F7AF);  { 10 }
  FF(C, D, A, B, InA [10], S13, $FFFF5BB1);  { 11 }
  FF(B, C, D, A, InA [11], S14, $895CD7BE);  { 12 }
  FF(A, B, C, D, InA [12], S11, $6B901122);  { 13 }
  FF(D, A, B, C, InA [13], S12, $FD987193);  { 14 }
  FF(C, D, A, B, InA [14], S13, $A679438E);  { 15 }
  FF(B, C, D, A, InA [15], S14, $49B40821);  { 16 }

  {round 2}
  GG(A, B, C, D, InA [ 1], S21, $F61E2562);  { 17 }
  GG(D, A, B, C, InA [ 6], S22, $C040B340);  { 18 }
  GG(C, D, A, B, InA [11], S23, $265E5A51);  { 19 }
  GG(B, C, D, A, InA [ 0], S24, $E9B6C7AA);  { 20 }
  GG(A, B, C, D, InA [ 5], S21, $D62F105D);  { 21 }
  GG(D, A, B, C, InA [10], S22, $02441453);  { 22 }
  GG(C, D, A, B, InA [15], S23, $D8A1E681);  { 23 }
  GG(B, C, D, A, InA [ 4], S24, $E7D3FBC8);  { 24 }
  GG(A, B, C, D, InA [ 9], S21, $21E1CDE6);  { 25 }
  GG(D, A, B, C, InA [14], S22, $C33707D6);  { 26 }
  GG(C, D, A, B, InA [ 3], S23, $F4D50D87);  { 27 }
  GG(B, C, D, A, InA [ 8], S24, $455A14ED);  { 28 }
  GG(A, B, C, D, InA [13], S21, $A9E3E905);  { 29 }
  GG(D, A, B, C, InA [ 2], S22, $FCEFA3F8);  { 30 }
  GG(C, D, A, B, InA [ 7], S23, $676F02D9);  { 31 }
  GG(B, C, D, A, InA [12], S24, $8D2A4C8A);  { 32 }

  {round 3}
  HH(A, B, C, D, InA [ 5], S31, $FFFA3942);  { 33 }
  HH(D, A, B, C, InA [ 8], S32, $8771F681);  { 34 }
  HH(C, D, A, B, InA [11], S33, $6D9D6122);  { 35 }
  HH(B, C, D, A, InA [14], S34, $FDE5380C);  { 36 }
  HH(A, B, C, D, InA [ 1], S31, $A4BEEA44);  { 37 }
  HH(D, A, B, C, InA [ 4], S32, $4BDECFA9);  { 38 }
  HH(C, D, A, B, InA [ 7], S33, $F6BB4B60);  { 39 }
  HH(B, C, D, A, InA [10], S34, $BEBFBC70);  { 40 }
  HH(A, B, C, D, InA [13], S31, $289B7EC6);  { 41 }
  HH(D, A, B, C, InA [ 0], S32, $EAA127FA);  { 42 }
  HH(C, D, A, B, InA [ 3], S33, $D4EF3085);  { 43 }
  HH(B, C, D, A, InA [ 6], S34,  $4881D05);  { 44 }
  HH(A, B, C, D, InA [ 9], S31, $D9D4D039);  { 45 }
  HH(D, A, B, C, InA [12], S32, $E6DB99E5);  { 46 }
  HH(C, D, A, B, InA [15], S33, $1FA27CF8);  { 47 }
  HH(B, C, D, A, InA [ 2], S34, $C4AC5665);  { 48 }

  {round 4}
  II(A, B, C, D, InA [ 0], S41, $F4292244);  { 49 }
  II(D, A, B, C, InA [ 7], S42, $432AFF97);  { 50 }
  II(C, D, A, B, InA [14], S43, $AB9423A7);  { 51 }
  II(B, C, D, A, InA [ 5], S44, $FC93A039);  { 52 }
  II(A, B, C, D, InA [12], S41, $655B59C3);  { 53 }
  II(D, A, B, C, InA [ 3], S42, $8F0CCC92);  { 54 }
  II(C, D, A, B, InA [10], S43, $FFEFF47D);  { 55 }
  II(B, C, D, A, InA [ 1], S44, $85845DD1);  { 56 }
  II(A, B, C, D, InA [ 8], S41, $6FA87E4F);  { 57 }
  II(D, A, B, C, InA [15], S42, $FE2CE6E0);  { 58 }
  II(C, D, A, B, InA [ 6], S43, $A3014314);  { 59 }
  II(B, C, D, A, InA [13], S44, $4E0811A1);  { 60 }
  II(A, B, C, D, InA [ 4], S41, $F7537E82);  { 61 }
  II(D, A, B, C, InA [11], S42, $BD3AF235);  { 62 }
  II(C, D, A, B, InA [ 2], S43, $2AD7D2BB);  { 63 }
  II(B, C, D, A, InA [ 9], S44, $EB86D391);  { 64 }

  Inc(Buf [0], A);
  Inc(Buf [1], B);
  Inc(Buf [2], C);
  Inc(Buf [3], D);

  ALMove(Buf, Buffer, SizeOf(Buffer));                                 
end;

{***********************************************************}
procedure ALCipherInitMD5(var Context : TALCipherMD5Context);
var
  MD5 : TALCipherMD5ContextEx;                                               
begin
  ALMove(Context, MD5, SizeOf(MD5));
  MD5.Count[0] := 0;
  MD5.Count[1] := 0;

  {load magic initialization constants}
  MD5.State[0] := $67452301;
  MD5.State[1] := $EFCDAB89;
  MD5.State[2] := $98BADCFE;
  MD5.State[3] := $10325476;
  ALMove(MD5, Context, SizeOf(Context));                               
end;

{*********************************************************************************************}
procedure ALCipherUpdateMD5(var Context : TALCipherMD5Context;  const Buf;  BufSize : LongInt);
var
  MD5    : TALCipherMD5ContextEx;
  InBuf  : array [0..15] of DWord;
  BufOfs : LongInt;
  MDI    : Word;
  I      : Word;
  II     : Word;
begin
  ALMove(Context, MD5, SizeOf(MD5));                                   

  {compute number of bytes mod 64}
  MDI := (MD5.Count[0] shr 3) and $3F;

  {update number of bits}
  if ((MD5.Count[0] + (DWord(BufSize) shl 3)) < MD5.Count[0]) then
    Inc(MD5.Count[1]);
  Inc(MD5.Count[0], BufSize shl 3);
  Inc(MD5.Count[1], BufSize shr 29);

  {add new byte acters to buffer}
  BufOfs := 0;
  while (BufSize > 0) do begin
    Dec(BufSize);
    MD5.Buf[MDI] := TByteArray(Buf)[BufOfs];                         
    Inc(MDI);
    Inc(BufOfs);
    if (MDI = $40) then begin
      II := 0;
      for I := 0 to 15 do begin
        InBuf[I] := LongInt(MD5.Buf[II + 3]) shl 24 or
          LongInt(MD5.Buf[II + 2]) shl 16 or
          LongInt(MD5.Buf[II + 1]) shl 8 or
          LongInt(MD5.Buf[II]);
        Inc(II, 4);
      end;
      ALCipherTransform(MD5.State, InBuf);
      ALCipherTransform(TALCipherMD5ContextEx( Context ).State, InBuf);
      MDI := 0;
    end;
  end;
  ALMove(MD5, Context, SizeOf(Context));
end;

{************************************************************************************************}
procedure ALCipherFinalizeMD5(var Context : TALCipherMD5Context; var Digest : TALCipherMD5Digest);
const
  Padding: array [0..63] of Byte = (
    $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);
var
  MD5    : TALCipherMD5ContextEx;
  InBuf  : array [0..15] of DWord;
  MDI    : LongInt;
  I      : Word;
  II     : Word;
  PadLen : Word;
begin
  ALMove(Context, MD5, SizeOf(MD5));                                   
  {save number of bits}
  InBuf[14] := MD5.Count[0];
  InBuf[15] := MD5.Count[1];
  {compute number of bytes mod 64}
  MDI := (MD5.Count[0] shr 3) and $3F;
  {pad out to 56 mod 64}
  if (MDI < 56) then
    PadLen := 56 - MDI
  else
    PadLen := 120 - MDI;
  ALCipherUpdateMD5(Context, Padding, PadLen);

  ALMove(Context, MD5, SizeOf(MD5));                                   

  {append length in bits and transform}
  II := 0;
  for I := 0 to 13 do begin
    InBuf[I] :=
      ( LongInt( MD5.Buf[ II + 3 ]) shl 24 ) or
      ( LongInt( MD5.Buf[ II + 2 ]) shl 16 ) or
      ( LongInt( MD5.Buf[ II + 1 ]) shl 8  ) or
        LongInt( MD5.Buf[ II     ]);
    Inc(II, 4);
  end;
  ALCipherTransform(MD5.State, InBuf);
  {store buffer in digest}
  II := 0;
  for I := 0 to 3 do begin
    Digest[II] := Byte(MD5.State[I] and $FF);
    Digest[II + 1] := Byte((MD5.State[I] shr 8) and $FF);
    Digest[II + 2] := Byte((MD5.State[I] shr 16) and $FF);
    Digest[II + 3] := Byte((MD5.State[I] shr 24) and $FF);
    Inc(II, 4);
  end;
  ALMove(MD5, Context, SizeOf(Context));                               
end;

{****************************************************************************************}
procedure ALCipherHashMD5(var Digest : TALCipherMD5Digest; const Buf;  BufSize : LongInt);
var
  Context : TALCipherMD5Context;
begin
  fillchar( context, SizeOf( context ), $00 );
  ALCipherInitMD5(Context);
  ALCipherUpdateMD5(Context, Buf, BufSize);
  ALCipherFinalizeMD5(Context, Digest);
end;

{********************************************************************************}
procedure ALCipherHashMix128(var Digest : LongInt; const Buf;  BufSize : LongInt);
type
  T128BitArray = array[0..0] of TALCipher128Bit;
var
  Temp      : TALCipher128Bit;
  PTemp     : PByteArray;
  I, L   : LongInt;
begin
  Temp[0] := $243F6A88;  {first 16 bytes of Pi in binary}
  Temp[1] := $93F40317;
  Temp[2] := $0C110496;
  Temp[3] := $C709C289;

  L := BufSize div SizeOf(TALCipher128Bit);
  for I := 0 to L - 1 do begin
    Temp[0] := Temp[0] + T128BitArray(Buf)[I][0];                    
    Temp[1] := Temp[1] + T128BitArray(Buf)[I][1];                    
    Temp[2] := Temp[2] + T128BitArray(Buf)[I][2];                    
    Temp[3] := Temp[3] + T128BitArray(Buf)[I][3];                    
    ALCipherMix128(Temp);
  end;

  PTemp := @Temp;
  if (BufSize > L * SizeOf(TALCipher128Bit)) then begin
    for I := 0 to (BufSize - L * SizeOf(TALCipher128Bit)) - 1 do
      PTemp^[I] := PTemp^[I] + TByteArray(Buf)[(L * SizeOf(TALCipher128Bit)) + I];
    ALCipherMix128(Temp);
  end;

  Digest := Temp[3];
end;

{*******************************************************************************}
procedure ALCipherXorMemPrim(var Mem1;  const Mem2;  Count : Cardinal); register;
{$IFDEF WIN64}
var
  PMem1, PMem2: PLongword;
  Rest: Integer;
begin
  Rest := Count and 3;
  Count := Count shr 2;
  PMem1 := PLongword(@Mem1);
  PMem2 := PLongword(@Mem2);

  // Xor main part in longword steps, count is already dvided by 4
  while Count > 0 do
  begin
    PMem1^ := PMem1^ xor PMem2^;
    Inc(PMem1);
    Inc(PMem2);
    Dec(Count);
  end;

  // Xor rest in byte steps.
  while Rest > 0 do
  begin
    PByte(PMem1)^ := PByte(PMem1)^ xor PByte(PMem2)^;
    Inc(PByte(PMem1));
    Inc(PByte(PMem2));
    Dec(Rest);
  end;
end;
{$ELSE}
asm
  push esi
  push edi

  mov  esi, eax         //esi = Mem1
  mov  edi, edx         //edi = Mem2

  push ecx              //save byte count
  shr  ecx, 2           //convert to dwords
  jz   @Continue

  cld
@Loop1:                 //xor dwords at a time
  mov  eax, [edi]
  xor  [esi], eax
  add  esi, 4
  add  edi, 4
  dec  ecx
  jnz  @Loop1

@Continue:              //handle remaining bytes (3 or less)
  pop  ecx
  and  ecx, 3
  jz   @Done

@Loop2:                 //xor remaining bytes
  mov  al, [edi]
  xor  [esi], al
  inc  esi
  inc  edi
  dec  ecx
  jnz  @Loop2

@Done:
  pop  edi
  pop  esi
end;
{$ENDIF}

{*****************************************************************}
procedure ALCipherXorMem(var Mem1;  const Mem2;  Count : Cardinal);
begin
  ALCipherXorMemPrim(Mem1, Mem2, Count);
end;

{***********************************************************************}
function AlCipherBufferToHex(const Buf; BufSize : Cardinal) : AnsiString;
var
  I     : LongInt;
begin
  Result := '';
  for I := 0 to BufSize - 1 do
    Result := Result + ALIntToHex(TByteArray(Buf)[I], 2);
end;

{*****SHA-1 hashing routines*************************************}
procedure ALCipherSHA1Clear( var Context : TALCipherSHA1Context );
begin
  fillchar( Context, SizeOf( Context ), $00 );
end;

{******************************************************}
function ALCipherSHA1SwapByteOrder( n : DWORD ) : DWORD;
begin
  n := ( n shr 24 ) or (( n shr 8 ) and cALCipherLBMASK_LO )
       or (( n shl 8 ) and cALCipherLBMASK_HI ) or ( n shl 24 );
  Result := n;
end;

{*******************************************************************************************}
procedure ALCipherHashSHA1( var Digest : TALCipherSHA1Digest; const Buf; BufSize : Longint );
var
  Context : TALCipherSHA1Context;
begin
  ALCipherInitSHA1( Context );
  ALCipherUpdateSHA1( Context, Buf, BufSize );
  ALCipherFinalizeSHA1( Context, Digest );
end;

{***************************************************************}
procedure ALCipherSHA1Hash( var Context : TALCipherSHA1Context );
var
  A : DWord;
  B : DWord;
  C : DWord;
  D : DWord;
  E : DWord;

  X : DWord;
  W : array[ 0..79 ] of DWord;

  i : Longint;
begin
  with Context do begin
    sdIndex:= 0;
    ALMove( sdBuf, W, Sizeof( W ));

    // W := Mt, for t = 0 to 15 : Mt is M sub t
    for i := 0 to 15 do
      W[ i ]:= ALCipherSHA1SwapByteOrder( W[ i ] );

    // Transform Message block from 16 32 bit words to 80 32 bit words
    // Wt, = ( Wt-3 xor Wt-8 xor Wt-13 xor Wt-16 ) rolL 1 : Wt is W sub t
    for i:= 16 to 79 do
      W[i]:= ALCipherRolX( W[ i - 3 ] xor W[ i - 8 ] xor W[ i - 14 ] xor W[ i - 16 ], 1 );

    A := sdHash[ 0 ];
    B := sdHash[ 1 ];
    C := sdHash[ 2 ];
    D := sdHash[ 3 ];
    E := sdHash[ 4 ];

    // the four rounds
    for i:= 0 to 19 do begin
      X := ALCipherRolX( A, 5 ) + ( D xor ( B and ( C xor D ))) + E + W[ i ] + cALCipherSHA1_K1;
      E := D;
      D := C;
      C := ALCipherRolX( B, 30 );
      B := A;
      A := X;
    end;

    for i:= 20 to 39 do begin
      X := ALCipherRolX( A, 5 ) + ( B xor C xor D ) + E + W[ i ] + cALCipherSHA1_K2;
      E := D;
      D := C;
      C := ALCipherRolX( B, 30 );
      B := A;
      A := X;
    end;

    for i:= 40 to 59 do begin
      X := ALCipherRolX( A, 5 ) + (( B and C ) or ( D and ( B or C ))) + E + W[ i ] + cALCipherSHA1_K3;
      E := D;
      D := C;
      C := ALCipherRolX( B, 30 );
      B := A;
      A := X;
    end;

    for i:= 60 to 79 do
    begin
      X := ALCipherRolX( A, 5 ) + ( B xor C xor D ) + E + W[ i ] + cALCipherSHA1_K4;
      E := D;
      D := C;
      C := ALCipherRolX( B, 30 );
      B := A;
      A := X;
    end;

    sdHash[ 0 ]:= sdHash[ 0 ] + A;
    sdHash[ 1 ]:= sdHash[ 1 ] + B;
    sdHash[ 2 ]:= sdHash[ 2 ] + C;
    sdHash[ 3 ]:= sdHash[ 3 ] + D;
    sdHash[ 4 ]:= sdHash[ 4 ] + E;

    FillChar( W, Sizeof( W ), $00 );
    FillChar( sdBuf, Sizeof( sdBuf ), $00 );
  end;
end;

{*********************************************************************************}
procedure ALCipherSHA1UpdateLen( var Context : TALCipherSHA1Context; Len : DWord );
begin
  Inc( Context.sdLo,( Len shl 3 ));
  if Context.sdLo < ( Len shl 3 ) then
    Inc( Context.sdHi );
  Inc( Context.sdHi, Len shr 29 );
end;

{***************************************************************}
procedure ALCipherInitSHA1( var Context : TALCipherSHA1Context );
begin
  ALCipherSHA1Clear( Context );
  Context.sdHash[ 0 ] := cALCipherSHA1_A;
  Context.sdHash[ 1 ] := cALCipherSHA1_B;
  Context.sdHash[ 2 ] := cALCipherSHA1_C;
  Context.sdHash[ 3 ] := cALCipherSHA1_D;
  Context.sdHash[ 4 ] := cALCipherSHA1_E;
end;

{**********************************************************************************************}
procedure ALCipherUpdateSHA1( var Context : TALCipherSHA1Context; const Buf; BufSize: Longint );
var
  PBuf: ^Byte;
begin
  with Context do begin
    ALCipherSHA1UpdateLen( Context, BufSize );
    PBuf := @Buf;
    while BufSize > 0 do begin
      if ( Sizeof( sdBuf ) - sdIndex ) <= DWord( BufSize ) then begin
        ALMove( PBuf^, sdBuf[ sdIndex ], Sizeof( sdBuf ) - sdIndex );
        Dec( BufSize, Sizeof( sdBuf ) - sdIndex );
        Inc( PBuf, Sizeof( sdBuf ) - sdIndex );
        ALCipherSHA1Hash( Context );
      end else begin
        ALMove( PBuf^, sdBuf[ sdIndex ], BufSize );
        Inc( sdIndex, BufSize );
        BufSize := 0;
      end;
    end;
  end;
end;

{*****************************************************************************************************}
procedure ALCipherFinalizeSHA1( var Context : TALCipherSHA1Context; var Digest : TALCipherSHA1Digest );
begin
  with Context do begin
    sdBuf[ sdIndex ] := $80;

    if sdIndex >= 56 then
      ALCipherSHA1Hash( Context );

    PDWord( @sdBuf[ 56 ])^ := ALCipherSHA1SwapByteOrder( sdHi );
    PDWord( @sdBuf[ 60 ])^ := ALCipherSHA1SwapByteOrder( sdLo );

    ALCipherSHA1Hash( Context );

    sdHash[ 0 ] := ALCipherSHA1SwapByteOrder( sdHash[ 0 ]);
    sdHash[ 1 ] := ALCipherSHA1SwapByteOrder( sdHash[ 1 ]);
    sdHash[ 2 ] := ALCipherSHA1SwapByteOrder( sdHash[ 2 ]);
    sdHash[ 3 ] := ALCipherSHA1SwapByteOrder( sdHash[ 3 ]);
    sdHash[ 4 ] := ALCipherSHA1SwapByteOrder( sdHash[ 4 ]);

    ALMove( sdHash, Digest, Sizeof( Digest ));
    ALCipherSHA1Clear( Context );
  end;
end;

{*****Rijndael************************************************************}
function ALCipherRdlSubVector(v : TALCipherRDLVector) : TALCipherRDLVector;
  { S-Box substitution }
begin
  Result.bt[0] := cALCipherRdlSBox[v.bt[0]];
  Result.bt[1] := cALCipherRdlSBox[v.bt[1]];
  Result.bt[2] := cALCipherRdlSBox[v.bt[2]];
  Result.bt[3] := cALCipherRdlSBox[v.bt[3]];
end;

{******************************************************************************************}
function ALCipherRdlRotateVector(v : TALCipherRDLVector; Count : Byte) : TALCipherRDLVector;
  { rotate vector (count bytes) to the right, e.g. }
  { |3 2 1 0| -> |0 3 2 1| for Count = 1 }
var
  i : Byte;
begin
  i := Count mod 4;
  Result.bt[0] := v.bt[i];
  Result.bt[1] := v.bt[(i+1) mod 4];
  Result.bt[2] := v.bt[(i+2) mod 4];
  Result.bt[3] := v.bt[(i+3) mod 4];
end;

{*************************************************************************************************************}
procedure ALCipherRdlRound(const RoundKey : TALCipherRDLBlock; var State : TALCipherRDLBlock; Final : Boolean);
  { Rijndael round transformation }
  { entire routine rewritten for optimization }                      
var
  i : Integer;
  e : TALCipherRDLVectors;
begin
  for i := 0 to 3 do begin
    if not Final then begin
      e[i].dw := cALCipherRDL_T0[TALCipherRDlVectors(State)[(i+0) mod 4].bt[0]] xor
                 cALCipherRDL_T1[TALCipherRDlVectors(State)[(i+1) mod 4].bt[1]] xor
                 cALCipherRDL_T2[TALCipherRDlVectors(State)[(i+2) mod 4].bt[2]] xor
                 cALCipherRDL_T3[TALCipherRDlVectors(State)[(i+3) mod 4].bt[3]]
    end else begin
      e[i].bt[0] := cALCipherRDLSBox[TALCipherRDlVectors(State)[(i+0) mod 4].bt[0]];
      e[i].bt[1] := cALCipherRDLSBox[TALCipherRDlVectors(State)[(i+1) mod 4].bt[1]];
      e[i].bt[2] := cALCipherRDLSBox[TALCipherRDlVectors(State)[(i+2) mod 4].bt[2]];
      e[i].bt[3] := cALCipherRDLSBox[TALCipherRDlVectors(State)[(i+3) mod 4].bt[3]];
    end;
  end;
  ALCipherXorMemPrim(e, RoundKey, SizeOf(TALCipherRDLBlock));
  State := TALCipherRDLBlock(e);
end;

{****************************************************************************************************************}
procedure ALCipherRdlInvRound(const RoundKey : TALCipherRDLBlock; var State : TALCipherRDLBlock; First : Boolean);
  { Rijndael inverse round transformation }
  { entire routine rewritten for optimization }                      
var
  i : Integer;
  r : TALCipherRDLVectors;
  e : TALCipherRDLVector;
begin
  ALCipherXorMemPrim(State, RoundKey, SizeOf(TALCipherRDLBlock));
  for i := 0 to 3 do begin
    if not First then begin
      e.dw := cALCipherRDL_InvT0[TALCipherRDlVectors(State)[i].bt[0]] xor
              cALCipherRDL_InvT1[TALCipherRDlVectors(State)[i].bt[1]] xor
              cALCipherRDL_InvT2[TALCipherRDlVectors(State)[i].bt[2]] xor
              cALCipherRDL_InvT3[TALCipherRDlVectors(State)[i].bt[3]];
      r[(i+0) mod 4].bt[0] := cALCipherRDLInvSBox[e.bt[0]];
      r[(i+1) mod 4].bt[1] := cALCipherRDLInvSBox[e.bt[1]];
      r[(i+2) mod 4].bt[2] := cALCipherRDLInvSBox[e.bt[2]];
      r[(i+3) mod 4].bt[3] := cALCipherRDLInvSBox[e.bt[3]];
    end else begin
      r[i].bt[0] := cALCipherRDLInvSBox[TALCipherRDlVectors(State)[(i+0) mod 4].bt[0]];
      r[i].bt[1] := cALCipherRDLInvSBox[TALCipherRDlVectors(State)[(i+3) mod 4].bt[1]];
      r[i].bt[2] := cALCipherRDLInvSBox[TALCipherRDlVectors(State)[(i+2) mod 4].bt[2]];
      r[i].bt[3] := cALCipherRDLInvSBox[TALCipherRDlVectors(State)[(i+1) mod 4].bt[3]];
    end;
  end;
  State := TALCipherRDLBlock(r);
end;

{***********************************************************************************************}
procedure ALCipherEncryptRDL(const Context : TALCipherRDLContext; var Block : TALCipherRDLBlock);
  { encrypt/decrypt block ECB mode }
var
  i : Integer;
begin
  if Context.Encrypt then begin
    ALCipherXorMemPrim(Block, Context.Rk[0], SizeOf(TALCipherRDLBlock));
    for i := 1 to (Context.Rounds - 1) do
      ALCipherRdlRound(Context.Rk[i], Block, False);
    ALCipherRdlRound(Context.Rk[Context.Rounds], Block, True);
  end else begin
    ALCipherRdlInvRound(Context.Rk[Context.Rounds], Block, True);
    for i := (Context.Rounds - 1) downto 1 do
      ALCipherRdlInvRound(Context.Rk[i], Block, False);
    ALCipherXorMemPrim(Block, Context.Rk[0], SizeOf(TALCipherRDLBlock));
  end;
end;

{**********************************************************************************************************************************}
procedure ALCipherEncryptRDLCBC(const Context : TALCipherRDLContext; const Prev : TALCipherRDLBlock; var Block : TALCipherRDLBlock);
  { encrypt/decrypt block CBC mode }
begin
  if Context.Encrypt then begin
    ALCipherXorMemPrim(Block, Prev, SizeOf(Block));
    ALCipherEncryptRDL(Context, Block);
  end else begin
    ALCipherEncryptRDL(Context, Block);
    ALCipherXorMemPrim(Block, Prev, SizeOf(Block));
  end;
end;

{*******************************************************************************************************************}
procedure ALCipherInitEncryptRDL(const Key; KeySize : Longint; var Context : TALCipherRDLContext; Encrypt : Boolean);
  { Rijndael key expansion }
var
  i : Integer;
  Nk : Byte;
  temp : TALCipherRDLVector;
  Sk : Longint;
begin
  { prepare context }
  FillChar(Context, SizeOf(Context), #0);
  Context.Encrypt := Encrypt;
  Sk := Min(KeySize, SizeOf(Context.Rk));
  ALMove(Key, Context.Rk, Sk);
  Nk := KeySize div 4;       { # key columns }
  if (Nk > cALCipherRDLNk256) then
    Nk := cALCipherRDLNk256
  else if (Nk < cALCipherRDLNk128) then
    Nk := cALCipherRDLNk128;
  Context.Rounds := 6 + Nk;

  { expand key into round keys }
  for i := Nk to (4 * (Context.Rounds + 1)) do begin
    temp := Context.W[i-1];
    if (Nk in [cALCipherRDLNk128, cALCipherRDLNk192]) then
      begin
        if (i mod Nk) = 0 then
          temp.dw := ALCipherRdlSubVector(ALCipherRdlRotateVector(temp, 1)).dw xor cALCipherRCon[i div Nk];
        Context.W[i].dw := Context.W[i - Nk].dw xor temp.dw;
      end
    else  { Nk = RDLNk256 }
      begin
        if (i mod Nk) = 0 then
          temp.dw := ALCipherRdlSubVector(ALCipherRdlRotateVector(temp, 1)).dw xor cALCipherRCon[i div Nk]
        else if (i mod Nk) = 4 then
          temp := ALCipherRdlSubVector(Temp);
        Context.W[i].dw := Context.W[i - Nk].dw xor temp.dw;
      end;
  end;
end;






















{***}
const
  cAlCryptInvalidFileFormat = 'Invalid file format';

{********************************************************************************************}
function ALXorEncrypt(const InString:AnsiString; StartKey,MultKey,AddKey:Integer): AnsiString;
var c : Byte;
    I:Integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    C := Byte(InString[I]) xor (StartKey shr 8);
    Result := Result + ALIntToHex(c ,2);
    StartKey := (c + StartKey) * MultKey + AddKey;
  end;
end;

{********************************************************************************************}
function ALXorDecrypt(const InString:AnsiString; StartKey,MultKey,AddKey:Integer): AnsiString;
Var C : AnsiChar;
    I:Integer;
begin
  Result := '';
  I := 1;
  While I < Length(InString) do begin
    C := AnsiChar(ALStrToInt('$' + ALCopyStr(InString,I,2)));
    Result := Result + AnsiChar(Byte(C) xor (StartKey shr 8));
    StartKey := (Byte(C) + StartKey) * MultKey + AddKey;
    Inc(i,2);
  end;
end;

{*****Blowfish****************************************}
procedure ALBFEncryptString(const InString: AnsiString;
                            var OutString: AnsiString;
                            const Key: TALCipherKey128;
                            Encrypt : Boolean);
begin
  OutString := ALBFEncryptString(InString, Key, Encrypt);
end;

{********************************************************}
procedure ALBFEncryptStringCBC(const InString: AnsiString;
                               var OutString: AnsiString;
                               const Key: TALCipherKey128;
                               Encrypt : Boolean);
begin
  OutString := ALBFEncryptStringCBC(InString, Key, Encrypt);
end;

{****************************************************}
function ALBFEncryptString(const InString: AnsiString;
                           const Key: TALCipherKey128;
                           Encrypt : Boolean): AnsiString;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
    InStream.Write(InString[1], Length(InString));
    InStream.Position := 0;

    if Encrypt then ALBFEncryptStream(InStream, OutStream, Key, True)
    else ALBFEncryptStream(InStream, OutStream, Key, False);
    OutStream.Position := 0;
    SetLength(Result, OutStream.Size);
    OutStream.Read(Result[1], OutStream.Size);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{*******************************************************}
function ALBFEncryptStringCBC(const InString: AnsiString;
                              const Key: TALCipherKey128;
                              Encrypt : Boolean) : AnsiString;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  Try
    InStream.Write(InString[1], Length(InString));
    InStream.Position := 0;

    if Encrypt then ALBFEncryptStreamCBC(InStream, OutStream, Key, True)
    else ALBFEncryptStreamCBC(InStream, OutStream, Key, False);
    OutStream.Position := 0;
    SetLength(Result, OutStream.Size);
    OutStream.Read(Result[1], OutStream.Size);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{************************************************************************************************************}
procedure AlBFEncryptFile(const InFile, OutFile : AnsiString; const Key : TALCipherKey128; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(String(InFile), fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(String(OutFile), fmCreate);
    try
      ALBFEncryptStream(InStream, OutStream, Key, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{***************************************************************************************************************}
procedure AlBFEncryptFileCBC(const InFile, OutFile : AnsiString; const Key : TALCipherKey128; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(String(InFile), fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(String(OutFile), fmCreate);
    try
      ALBFEncryptStreamCBC(InStream, OutStream, Key, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{*********************************************************************************************************}
procedure AlBFEncryptStream(InStream, OutStream : TStream; const Key : TALCipherKey128; Encrypt : Boolean);
var
  I          : LongInt;
  Block      : TALCipherBFBlock;
  Context    : TALCipherBFContext;
  BlockCount : LongInt;
begin
  ALCipherInitEncryptBF(Key, Context);

  {get the number of blocks in the file}
  BlockCount := (InStream.Size div SizeOf(Block));

  {when encrypting, make sure we have a block with at least one free}
  {byte at the end. used to store the odd byte count value}
  if Encrypt then
    Inc(BlockCount);

  {process all except the last block}
  for I := 1 to BlockCount - 1 do begin
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cAlCryptInvalidFileFormat);
    ALCipherEncryptBF(Context, Block, Encrypt);
    OutStream.Write(Block, SizeOf(Block));
  end;

  if Encrypt then begin
    FillChar(Block, SizeOf(Block), #0);

    {set actual number of bytes to read}
    I := (InStream.Size mod SizeOf(Block));
    if InStream.Read(Block, I) <> I then
      raise EAlCipherException.Create(cALCryptInvalidFileFormat);

    {store number of bytes as last byte in last block}
    PByteArray(@Block)^[SizeOf(Block)-1] := I;

    {encrypt and save full block}
    ALCipherEncryptBF(Context, Block, Encrypt);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptBF(Context, Block, Encrypt);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
end;

{************************************************************************************************************}
procedure AlBFEncryptStreamCBC(InStream, OutStream : TStream; const Key : TALCipherKey128; Encrypt : Boolean);
var
  I : LongInt;
  Block : TALCipherBFBlock;
  IV : TALCipherBFBlock;
  Work : TALCipherBFBlock;
  Context : TALCipherBFContext;
  BlockCount : LongInt;
begin
  ALCipherInitEncryptBF(Key, Context);

  {get the number of blocks in the file}
  BlockCount := (InStream.Size div SizeOf(Block));

  if Encrypt then begin
    {set up an initialization vector (IV)}
    Block[0] := timeGetTime;
    Block[1] := timeGetTime;
    ALCipherEncryptBF(Context, Block, Encrypt);
    OutStream.Write(Block, SizeOf(Block));
    IV := Block;
  end else begin
    {read the frist block to prime the IV}
    InStream.Read(Block, SizeOf(Block));
    Dec(BlockCount);
    IV := Block;
  end;

  {when encrypting, make sure we have a block with at least one free}
  {byte at the end. used to store the odd byte count value}
  if Encrypt then
    Inc(BlockCount);

  {process all except the last block}
  for I := 1 to BlockCount - 1 do begin
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);

    if Encrypt then begin
      ALCipherEncryptBFCBC(Context, IV, Block, Encrypt);
      IV := Block;
    end else begin
      Work := Block;
      ALCipherEncryptBFCBC(Context, IV, Block, Encrypt);
      IV := Work;
    end;

    OutStream.Write(Block, SizeOf(Block));
  end;

  if Encrypt then begin
    FillChar(Block, SizeOf(Block), #0);

    {set actual number of bytes to read}
    I := (InStream.Size mod SizeOf(Block));
    if InStream.Read(Block, I) <> I then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);

    {store number of bytes as last byte in last block}
    PByteArray(@Block)^[SizeOf(Block)-1] := I;

    {encrypt and save full block}
    ALCipherEncryptBFCBC(Context, IV, Block, Encrypt);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptBFCBC(Context, IV, Block, Encrypt);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
end;

{*****************************************************}
procedure ALBFEncryptString(const InString: AnsiString;
                            var OutString : AnsiString;
                            const Key: AnsiString;
                            Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptString(InString,OutString, aCipherKey128, Encrypt);
end;

{********************************************************}
procedure ALBFEncryptStringCBC(const InString: AnsiString;
                               var OutString : AnsiString;
                               const Key: AnsiString;
                               Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptStringCBC(InString, OutString, aCipherKey128, Encrypt);
end;

{*****************************************************}
function  AlBFEncryptString(const InString: AnsiString;
                            const Key: AnsiString;
                            Encrypt : Boolean) : AnsiString;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Result := AlBFEncryptString(InString, aCipherKey128, Encrypt);
end;

{********************************************************}
function  ALBFEncryptStringCBC(const InString: AnsiString;
                               const Key: AnsiString;
                               Encrypt : Boolean) : AnsiString;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  result := ALBFEncryptStringCBC(InString, aCipherKey128, Encrypt);
end;

{*******************************************************}
procedure ALBFEncryptStream(InStream, OutStream: TStream;
                            const Key: AnsiString;
                            Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptStream(InStream, OutStream, aCipherKey128, Encrypt);
end;

{**********************************************************}
procedure ALBFEncryptStreamCBC(InStream, OutStream: TStream;
                               const Key: AnsiString;
                               Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptStreamCBC(InStream, OutStream, aCipherKey128, Encrypt);
end;

{**********************************************************}
procedure ALBFEncryptFile(const InFile, OutFile: AnsiString;
                          const Key: AnsiString;
                          Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptFile(InFile, OutFile, aCipherKey128, Encrypt);
end;

{*************************************************************}
procedure ALBFEncryptFileCBC(const InFile, OutFile: AnsiString;
                             const Key: AnsiString;
                             Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptFileCBC(InFile, OutFile, aCipherKey128, Encrypt);
end;

{*****Rijndael*****************************************}
procedure AlRDLEncryptString(const InString: AnsiString;
                             var OutString: AnsiString;
                             const Key;
                             KeySize : Longint;
                             Encrypt : Boolean);
begin
  OutString := AlRDLEncryptString(InString, Key, KeySize, Encrypt);
end;

{*********************************************************}
procedure AlRDLEncryptStringCBC(const InString: AnsiString;
                                var OutString: AnsiString;
                                const Key;
                                KeySize : Longint;
                                Encrypt : Boolean);
begin
  OutString := AlRDLEncryptStringCBC(InString, Key, KeySize, Encrypt);
end;

{*****************************************************}
function AlRDLEncryptString(const InString: AnsiString;
                            const Key;
                            KeySize : Longint;
                            Encrypt : Boolean): AnsiString;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
    InStream.Write(InString[1], Length(InString));
    InStream.Position := 0;

    if Encrypt then AlRDLEncryptStream(InStream, OutStream, Key, KeySize, True)
    else AlRDLEncryptStream(InStream, OutStream, Key, KeySize, False);
    OutStream.Position := 0;
    SetLength(Result, OutStream.Size);
    OutStream.Read(Result[1], OutStream.Size);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{********************************************************}
function AlRDLEncryptStringCBC(const InString: AnsiString;
                               const Key;
                               KeySize : Longint;
                               Encrypt : Boolean) : AnsiString;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  Try
    InStream.Write(InString[1], Length(InString));
    InStream.Position := 0;

    if Encrypt then AlRDLEncryptStreamCBC(InStream, OutStream, Key, KeySize, True)
    else AlRDLEncryptStreamCBC(InStream, OutStream, Key, KeySize, False);
    OutStream.Position := 0;
    SetLength(Result, OutStream.Size);
    OutStream.Read(Result[1], OutStream.Size);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{**************************************************************************************************************}
procedure AlRDLEncryptFile(const InFile, OutFile : AnsiString; const Key; KeySize : Longint; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(String(InFile), fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(String(OutFile), fmCreate);
    try
      ALRDLEncryptStream(InStream, OutStream, Key, KeySize, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{*****************************************************************************************************************}
procedure AlRDLEncryptFileCBC(const InFile, OutFile : AnsiString; const Key; KeySize : Longint; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(String(InFile), fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(String(OutFile), fmCreate);
    try
      ALRDLEncryptStreamCBC(InStream, OutStream, Key, KeySize, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{***********************************************************************************************************}
procedure AlRDLEncryptStream(InStream, OutStream : TStream; const Key; KeySize : Longint; Encrypt : Boolean);
var
  I          : LongInt;
  Block      : TALCipherRDLBlock;
  Context    : TALCipherRDLContext;
  BlockCount : LongInt;
begin
  ALCipherInitEncryptRDL(Key, KeySize, Context, Encrypt);

  {get the number of blocks in the file}
  BlockCount := (InStream.Size div SizeOf(Block));

  {when encrypting, make sure we have a block with at least one free}
  {byte at the end. used to store the odd byte count value}
  if Encrypt then
    Inc(BlockCount);

  {process all except the last block}
  for I := 1 to BlockCount - 1 do begin
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptRDL(Context, Block);
    OutStream.Write(Block, SizeOf(Block));
  end;

  if Encrypt then begin
    FillChar(Block, SizeOf(Block), #0);

    {set actual number of bytes to read}
    I := (InStream.Size mod SizeOf(Block));
    if InStream.Read(Block, I) <> I then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);

    {store number of bytes as last byte in last block}
    PByteArray(@Block)^[SizeOf(Block)-1] := I;

    {encrypt and save full block}
    ALCipherEncryptRDL(Context, Block);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptRDL(Context, Block);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
end;

{**************************************************************************************************************}
procedure AlRDLEncryptStreamCBC(InStream, OutStream : TStream; const Key; KeySize : Longint; Encrypt : Boolean);
var
  I          : LongInt;
  Block      : TALCipherRDLBlock;
  IV         : TALCipherRDLBlock;
  Work       : TALCipherRDLBlock;
  Context    : TALCipherRDLContext;
  BlockCount : LongInt;
begin
  ALCipherInitEncryptRDL(Key, KeySize, Context, Encrypt);

  {get the number of blocks in the file}
  BlockCount := (InStream.Size div SizeOf(Block));

  if Encrypt then begin
    {set up an initialization vector (IV)}
    Block[0] := timeGetTime;
    Block[1] := timeGetTime;
    ALCipherEncryptRDL(Context, Block);
    OutStream.Write(Block, SizeOf(Block));
    IV := Block;
  end else begin
    {read the frist block to prime the IV}
    InStream.Read(Block, SizeOf(Block));
    Dec(BlockCount);
    IV := Block;
  end;

  {when encrypting, make sure we have a block with at least one free}
  {byte at the end. used to store the odd byte count value}
  if Encrypt then
    Inc(BlockCount);

  {process all except the last block}
  for I := 1 to BlockCount - 1 do begin
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);

    if Encrypt then begin
      ALCipherEncryptRDLCBC(Context, IV, Block);
      IV := Block;
    end else begin
      Work := Block;
      ALCipherEncryptRDLCBC(Context, IV, Block);
      IV := Work;
    end;

    OutStream.Write(Block, SizeOf(Block));
  end;

  if Encrypt then begin
    FillChar(Block, SizeOf(Block), #0);

    {set actual number of bytes to read}
    I := (InStream.Size mod SizeOf(Block));
    if InStream.Read(Block, I) <> I then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);

    {store number of bytes as last byte in last block}
    PByteArray(@Block)^[SizeOf(Block)-1] := I;

    {encrypt and save full block}
    ALCipherEncryptRDLCBC(Context, IV, Block);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptRDLCBC(Context, IV, Block);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
end;

{******************************************************}
procedure AlRDLEncryptString(const InString: AnsiString;
                             var OutString : AnsiString;
                             const Key: AnsiString;
                             Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptString(InString,OutString, aCipherKey128, 16, Encrypt);
end;

{*********************************************************}
procedure AlRDLEncryptStringCBC(const InString: AnsiString;
                                var OutString : AnsiString;
                                const Key: AnsiString;
                                Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptStringCBC(InString, OutString, aCipherKey128, 16, Encrypt);
end;

{******************************************************}
function  AlRDLEncryptString(const InString: AnsiString;
                             const Key: AnsiString;
                             Encrypt : Boolean) : AnsiString;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Result := AlRDLEncryptString(InString, aCipherKey128, 16, Encrypt);
end;

{*********************************************************}
function  AlRDLEncryptStringCBC(const InString: AnsiString;
                                const Key: AnsiString;
                                Encrypt : Boolean) : AnsiString;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  result := AlRDLEncryptStringCBC(InString, aCipherKey128, 16, Encrypt);
end;

{********************************************************}
procedure AlRDLEncryptStream(InStream, OutStream: TStream;
                             const Key: AnsiString;
                             Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptStream(InStream, OutStream, aCipherKey128, 16, Encrypt);
end;

{***********************************************************}
procedure AlRDLEncryptStreamCBC(InStream, OutStream: TStream;
                                const Key: AnsiString;
                                Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptStreamCBC(InStream, OutStream, aCipherKey128, 16, Encrypt);
end;

{***********************************************************}
procedure AlRDLEncryptFile(const InFile, OutFile: AnsiString;
                           const Key: AnsiString;
                           Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptFile(InFile, OutFile, aCipherKey128, 16, Encrypt);
end;

{**************************************************************}
procedure AlRDLEncryptFileCBC(const InFile, OutFile: AnsiString;
                              const Key: AnsiString;
                              Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptFileCBC(InFile, OutFile, aCipherKey128, 16, Encrypt);
end;

{****MD5******************************************************************************}
procedure AlFileHashMD5(var Digest : TALCipherMD5Digest; const AFileName : AnsiString);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(String(AFileName), fmOpenRead);
  try
    ALStreamHashMD5(Digest, FS);
  finally
    FS.Free;
  end;
end;

{***************************************************************}
function ALFileHashMD5(const AFileName : AnsiString): AnsiString;
Var aMD5Digest: TAlCipherMD5Digest;
Begin
  ALFileHashMD5(aMD5Digest, AFileName);
  Result := AlCipherBufferToHex(aMD5Digest, SizeOf(aMD5Digest));
end;

{****************************************************************************}
procedure AlStreamHashMD5(var Digest : TALCipherMD5Digest; AStream : TStream);
var
  BufSize : Cardinal;
  Buf : array[0..1023] of Byte;
  Context : TALCipherMD5Context;
begin
  ALCipherInitMD5(Context);
  BufSize := AStream.Read(Buf, SizeOf(Buf));
  while (BufSize > 0) do begin
    ALCipherUpdateMD5(Context, Buf, BufSize);
    BufSize := AStream.Read(Buf, SizeOf(Buf));
  end;
  ALCipherFinalizeMD5(Context, Digest);
end;

{******************************************************}
function AlStreamHashMD5(AStream : TStream): AnsiString;
Var aMD5Digest: TAlCipherMD5Digest;
Begin
  AlStreamHashMD5(aMD5Digest, AStream);
  Result := AlCipherBufferToHex(aMD5Digest, SizeOf(aMD5Digest));
end;

{****SHA1*******************************************************************************}
procedure AlFileHashSHA1(var Digest : TALCipherSHA1Digest; const AFileName : AnsiString);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(String(AFileName), fmOpenRead);
  try
    ALStreamHashSHA1(Digest, FS);
  finally
    FS.Free;
  end;
end;

{*****************************************************************}
function  ALFileHashSHA1(const AFileName : AnsiString): AnsiString;
Var aSHA1Digest: TALCipherSHA1Digest;
Begin
  ALFileHashSHA1(aSHA1Digest, AFileName);
  Result := AlCipherBufferToHex(aSHA1Digest, SizeOf(aSHA1Digest));
end;

{******************************************************************************}
procedure AlStreamHashSHA1(var Digest : TALCipherSHA1Digest; AStream : TStream);
var
  BufSize : Cardinal;
  Buf : array[0..1023] of Byte;
  Context : TALCipherSHA1Context;
begin
  ALCipherInitSHA1(Context);
  BufSize := AStream.Read(Buf, SizeOf(Buf));
  while (BufSize > 0) do begin
    ALCipherUpdateSHA1(Context, Buf, BufSize);
    BufSize := AStream.Read(Buf, SizeOf(Buf));
  end;
  ALCipherFinalizeSHA1(Context, Digest);
end;

{*******************************************************}
function AlStreamHashSHA1(AStream : TStream): AnsiString;
Var aSHA1Digest: TALCipherSHA1Digest;
Begin
  AlStreamHashSHA1(aSHA1Digest, AStream);
  Result := AlCipherBufferToHex(aSHA1Digest, SizeOf(aSHA1Digest));
end;

{**********************************************************************}
procedure ALStringHashELF(var Digest : LongInt; const Str : AnsiString);
begin
  ALCipherHashELF(Digest, Str[1], Length(Str));
end;

{********************************************************}
function ALStringHashELF(const Str : AnsiString): LongInt;
Begin
  AlStringHashELF(Result, str);
end;

{*************************************************************************}
procedure ALStringHashMix128(var Digest : LongInt; const Str : AnsiString);
begin
  ALCipherHashMix128(Digest, Str[1], Length(Str));
end;

{***********************************************************}
function ALStringHashMix128(const Str : AnsiString): LongInt;
Begin
  ALStringHashMix128(Result, Str);
end;

{*********************************************************************************}
procedure ALStringHashMD5(var Digest : TALCipherMD5Digest; const Str : AnsiString);
begin
  ALCipherHashMD5(Digest, Str[1], Length(Str));
end;

{***********************************************************}
function ALStringHashMD5(const Str : AnsiString): AnsiString;
Var aMD5Digest: TAlCipherMD5Digest;
Begin
  AlStringHashMD5(aMD5Digest, Str);
  Result := AlCipherBufferToHex(aMD5Digest, SizeOf(aMD5Digest));
end;

{***********************************************************************************}
procedure ALStringHashSHA1(var Digest : TALCipherSHA1Digest; const Str : AnsiString);
begin
  ALCipherHashSHA1(Digest, Str[1], Length(Str));
end;

{************************************************************}
function ALStringHashSHA1(const Str : AnsiString): AnsiString;
Var aSHA1Digest: TAlCipherSHA1Digest;
Begin
  AlStringHashSHA1(aSHA1Digest, Str);
  Result := AlCipherBufferToHex(ASHA1Digest, SizeOf(aSHA1Digest));
end;

end.
