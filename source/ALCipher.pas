{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    TurboPower Software
							Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALCipher (Private Key Encryption/Decryption Primitives)
Version:      3.50

Description:  Function to generate hash (using MD5 or SHA-1) from input

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

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

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALCipher;

interface

uses Windows,
     Classes,
     sysutils;

const
  cALCipherBFRounds = 16;      { 16 blowfish rounds }

type

  { Cipher exception }
  EALCipherException = class(Exception);

  { message digest blocks }
  TALCipherMD5Digest  = array [0..15] of Byte;         { 128 bits - MD5 }
  TALCipherSHA1Digest = array [0..19] of Byte;         { 160 bits - SHA-1 }

  { message digest context types }
  TALCipherMD5Context  = array [0..87] of Byte;        { MD5 }
  TALCipherSHA1Context = record                        { SHA-1 }
    sdHi    : DWord;
    sdLo    : DWord;
    sdIndex : DWord;
    sdHash  : array [0..4] of DWord;
    sdBuf   : array [0..63] of Byte;
  end;
  TALCipherBFBlock = array[0..1] of LongInt;           { BlowFish }
  TALCipherBFContext = packed record
    PBox    : array[0..(cALCipherBFRounds+1)] of LongInt;
    SBox    : array[0..3, 0..255] of LongInt;
  end;
  PALCipherKey128 = ^TALCipherKey128;
  TALCipherKey128 = array [0..15] of Byte;

{ Blowfish Cipher }
procedure ALCipherInitEncryptBF(Key : TALCipherKey128; var Context : TALCipherBFContext);
procedure ALCipherEncryptBF(const Context : TALCipherBFContext; var Block : TALCipherBFBlock; Encrypt : Boolean);
procedure ALCipherEncryptBFCBC(const Context : TALCipherBFContext; const Prev : TALCipherBFBlock; var Block : TALCipherBFBlock; Encrypt : Boolean);

{ MD5 message digest }
procedure ALCipherInitMD5(var Context : TALCipherMD5Context);
procedure ALCipherHashMD5(var Digest : TALCipherMD5Digest; const Buf; BufSize : LongInt);
procedure ALCipherFinalizeMD5(var Context : TALCipherMD5Context; var Digest : TALCipherMD5Digest);
procedure ALCipherUpdateMD5(var Context : TALCipherMD5Context; const Buf;  BufSize : LongInt);

{ SHA-1 message digest }
procedure ALCipherInitSHA1(var Context: TALCipherSHA1Context);
procedure ALCipherHashSHA1(var Digest : TALCipherSHA1Digest; const Buf; BufSize : Longint);
procedure ALCipherUpdateSHA1(var Context : TALCipherSHA1Context; const Buf; BufSize: Longint);
procedure ALCipherFinalizeSHA1(var Context: TALCipherSHA1Context; var Digest : TALCipherSHA1Digest);

{ Miscellaneous hash algorithms }
procedure ALCipherHashELF(var Digest : LongInt; const Buf;  BufSize : LongInt);
procedure ALCipherHashMix128(var Digest : LongInt; const Buf;  BufSize : LongInt);

{ String hashing }
procedure ALCipherStringHashELF(var Digest : LongInt; const Str : string); overload;
procedure ALCipherStringHashMD5(var Digest : TALCipherMD5Digest; const Str : string); overload;
procedure ALCipherStringHashMix128(var Digest : LongInt; const Str : string); overload;
procedure ALCipherStringHashSHA1(var Digest : TALCipherSHA1Digest; const Str : string); overload;
function  ALCipherStringHashELF(const Str : string): LongInt; overload;
function  ALCipherStringHashMD5(const Str : string): String; overload;
function  ALCipherStringHashMix128(const Str : string): LongInt; overload;
function  ALCipherStringHashSHA1(const Str : string): String; overload;

{key generation}
procedure GenerateMD5Key(var Key: TALCipherKey128; const Str : string);

implementation

type

  pALCipherMD5ContextEx = ^TALCipherMD5ContextEx;
  TALCipherMD5ContextEx = packed record
    Count : array [0..1] of DWord;  {number of bits handled mod 2^64}
    State : array [0..3] of DWord;  {scratch buffer}
    Buf   : array [0..63] of Byte;    {input buffer}
  end;

  TALCipherBFBlockEx = packed record
    Xl : array[0..3] of Byte;
    Xr : array[0..3] of Byte;
  end;

  TALCipher128Bit     = array [0..3] of DWord;


const

  { SHA-1 constants }
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

  cALCipherbf_P: array[0..(cALCipherBFRounds + 1)] of DWord =
  (
   $243F6A88, $85A308D3, $13198A2E, $03707344,
   $A4093822, $299F31D0, $082EFA98, $EC4E6C89,
   $452821E6, $38D01377, $BE5466CF, $34E90C6C,
   $C0AC29B7, $C97C50DD, $3F84D5B5, $B5470917,
   $9216D5D9, $8979FB1B
  );

  cALCipherbf_S: array[0..3, 0..255] of DWord =
  (
   ($D1310BA6, $98DFB5AC, $2FFD72DB, $D01ADFB7,
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
    $53B02D5D, $A99F8FA1, $08BA4799, $6E85076A
   ),
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
    $153E21E7, $8FB03D4A, $E6E39F2B, $DB83ADF7
   ),
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
    $D79A3234, $92638212, $670EFA8E, $406000E0
   ),
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
    $B74E6132, $CE77E25B, $578FDFE3, $3AC372E6
   )
  );

{*******************************************************************************}
procedure ALCipherXorMemPrim(var Mem1;  const Mem2;  Count : Cardinal); register;
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

{***************************************************************}
procedure ALCipherXorMem(var Mem1; const Mem2; Count : Cardinal);
begin
  ALCipherXorMemPrim(Mem1, Mem2, Count);
end;

{*******************************************************************}
function AlCipherBufferToHex(const Buf; BufSize : Cardinal) : string;
var
  I     : LongInt;
begin
  Result := '';
  for I := 0 to BufSize - 1 do
    Result := Result + IntToHex(TByteArray(Buf)[I], 2);
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

{************************************************************************}
procedure ALCipherStringHashELF(var Digest : LongInt; const Str : string);
begin
  ALCipherHashELF(Digest, Str[1], Length(Str));
end;

{**********************************************************}
function ALCipherStringHashELF(const Str : string): LongInt;
Begin
  AlCipherStringHashELF(Result, str);
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
  Move(Buffer, Buf, SizeOf(Buf));
  Move(InBuf, InA, SizeOf(InA));
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

  Move(Buf, Buffer, SizeOf(Buffer));
end;

{****************************************************}
procedure ALCipherInitEncryptBF(Key : TALCipherKey128;
                                var Context : TALCipherBFContext);
var
  I     : Integer;
  J     : Integer;
  K     : Integer;
  Data  : LongInt;
  Block : TALCipherBFBlock;
begin
  {initialize PArray}
  Move(cALCipherbf_P, Context.PBox, SizeOf(Context.PBox));
  {initialize SBox}
  Move(cALCipherbf_S, Context.SBox, SizeOf(Context.SBox));

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
    AlCipherEncryptBF(Context, Block, True);
    Context.PBox[I] := Block[0];
    Context.PBox[I+1] := Block[1];
    Inc(I, 2);
  until I > cAlCipherBFRounds+1;

  {continue the process, replacing the elements of the four S-boxes in}
  {order, with the output of the continuously changing Blowfish algorithm}

  for J := 0 to 3 do begin
    I := 0;
    repeat
      AlCipherEncryptBF(Context, Block, True);
      Context.SBox[J, I] := Block[0];
      Context.SBox[J, I+1] := Block[1];
      Inc(I, 2);
    until I > 255;
  end;

  {in total, 521 iterations are required to generate all required subkeys. }
end;

{*************************************************************}
procedure ALCipherEncryptBF(const Context : TALCipherBFContext;
                            var Block : TALCipherBFBlock;
                            Encrypt : Boolean);
var
  I : Integer;
  TmpBlock : TAlCipherBFBlockEx;
begin
  Move(Block, TmpBlock, SizeOf(TmpBlock));
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
  end
  else begin
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

{****************************************************************}
procedure ALCipherEncryptBFCBC(const Context : TALcipherBFContext;
                               const Prev : TALCipherBFBlock;
                               var Block : TALCipherBFBlock;
                               Encrypt : Boolean);
begin
  if Encrypt then begin
    ALCipherXorMem(Block, Prev, SizeOf(Block));
    ALCipherEncryptBF(Context, Block, Encrypt);
  end
  else begin
    ALCipherEncryptBF(Context, Block, Encrypt);
    ALCipherXorMem(Block, Prev, SizeOf(Block));
  end;
end;

{***********************************************************}
procedure ALCipherInitMD5(var Context : TALCipherMD5Context);
var
  MD5 : TALCipherMD5ContextEx;
begin
  Move(Context, MD5, SizeOf(MD5));
  MD5.Count[0] := 0;
  MD5.Count[1] := 0;

  {load magic initialization constants}
  MD5.State[0] := $67452301;
  MD5.State[1] := $EFCDAB89;
  MD5.State[2] := $98BADCFE;
  MD5.State[3] := $10325476;
  Move(MD5, Context, SizeOf(Context));
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
  Move(Context, MD5, SizeOf(MD5));

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
  Move(MD5, Context, SizeOf(Context));
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
  Move(Context, MD5, SizeOf(MD5));
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

  Move(Context, MD5, SizeOf(MD5));

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
  Move(MD5, Context, SizeOf(Context));
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

{***************************************************************************}
procedure ALCipherStringHashMix128(var Digest : LongInt; const Str : string);
begin
  ALCipherHashMix128(Digest, Str[1], Length(Str));
end;

{*************************************************************}
function ALCipherStringHashMix128(const Str : string): LongInt;
Begin
  ALCipherStringHashMix128(Result, Str);
end;

{***********************************************************************************}
procedure ALCipherStringHashMD5(var Digest : TALCipherMD5Digest; const Str : string);
begin
  ALCipherHashMD5(Digest, Str[1], Length(Str));
end;

{*********************************************************}
function ALCipherStringHashMD5(const Str : string): String;
Var aMD5Digest: TAlCipherMD5Digest;
Begin
  AlCipherStringHashMD5(aMD5Digest, Str);
  Result := AlCipherBufferToHex(aMD5Digest, SizeOf(aMD5Digest));
end;

{****************************************************************}
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

{*************************************************************************************}
procedure ALCipherStringHashSHA1(var Digest : TALCipherSHA1Digest; const Str : string);
begin
  ALCipherHashSHA1(Digest, Str[1], Length(Str));
end;

{**********************************************************}
function ALCipherStringHashSHA1(const Str : string): String;
Var aSHA1Digest: TAlCipherSHA1Digest;
Begin
  AlCipherStringHashSHA1(aSHA1Digest, Str);
  Result := AlCipherBufferToHex(ASHA1Digest, SizeOf(aSHA1Digest));
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
    Move( sdBuf, W, Sizeof( W ));

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
        Move( PBuf^, sdBuf[ sdIndex ], Sizeof( sdBuf ) - sdIndex );
        Dec( BufSize, Sizeof( sdBuf ) - sdIndex );
        Inc( PBuf, Sizeof( sdBuf ) - sdIndex );
        ALCipherSHA1Hash( Context );
      end else begin
        Move( PBuf^, sdBuf[ sdIndex ], BufSize );
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

    Move( sdHash, Digest, Sizeof( Digest ));
    ALCipherSHA1Clear( Context );
  end;
end;

{*********************************************************************}
procedure GenerateMD5Key(var Key: TALCipherKey128; const Str : string);
var D : TALCipherMD5Digest;
begin
  ALCipherHashMD5(D, Str[1], Length(Str));
  Key := TALCipherKey128(D);
end;

end.
