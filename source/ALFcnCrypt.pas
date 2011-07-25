{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe              
Author(s):    TurboPower Software
              Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe Crypt Functions
Version:      3.50

Description:  Toolkit for data encryption. contains routines for use with
              Borland Delphi. provides support for Blowfish, MD5,
              SHA-1, DES, triple- DES, Rijndael.

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

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
              10/10/2009: add aes functions

Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  voting on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALFcnCrypt;

interface

uses MMSystem,
     Classes,
     AlCipher;

{Xor function}
function  ALXorEncrypt(const InString:string; StartKey,MultKey,AddKey:Integer): string;
function  ALXorDecrypt(const InString:string; StartKey,MultKey,AddKey:Integer): string;

{blowfish}
procedure ALBFEncryptString(const InString: string; var OutString : string; const Key : TALCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptStringCBC(const InString: string; var OutString : string; const Key : TALCipherKey128; Encrypt : Boolean); overload;
function  AlBFEncryptString(const InString: string; const Key : TALCipherKey128; Encrypt : Boolean) : string; overload;
function  ALBFEncryptStringCBC(const InString: string; const Key : TALCipherKey128; Encrypt : Boolean) : string; overload;
procedure ALBFEncryptFile(const InFile, OutFile : string; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptFileCBC(const InFile, OutFile : string; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptStream(InStream, OutStream : TStream; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptStreamCBC(InStream, OutStream : TStream; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptString(const InString: string; var OutString : string; const Key: string; Encrypt : Boolean); overload;
procedure ALBFEncryptStringCBC(const InString: string; var OutString : string; const Key: string; Encrypt : Boolean); overload;
function  AlBFEncryptString(const InString: string; const Key: string; Encrypt : Boolean) : string; overload;
function  ALBFEncryptStringCBC(const InString: string; const Key: string; Encrypt : Boolean) : string; overload;
procedure ALBFEncryptStream(InStream, OutStream: TStream; const Key: string; Encrypt : Boolean); overload;
procedure ALBFEncryptStreamCBC(InStream, OutStream: TStream; const Key: string; Encrypt : Boolean); overload;
procedure ALBFEncryptFile(const InFile, OutFile: string; const Key: string; Encrypt : Boolean); overload;
procedure ALBFEncryptFileCBC(const InFile, OutFile: string; const Key: string; Encrypt : Boolean); overload;

{DES}
procedure ALDESEncryptString(const InString: string; var OutString : string; const Key : TAlCipherKey64; Encrypt : Boolean); overload;
procedure ALDESEncryptStringCBC(const InString: string; var OutString : string; const Key : TAlCipherKey64; Encrypt : Boolean); overload;
function  ALDESEncryptString(const InString: string; const Key : TAlCipherKey64; Encrypt : Boolean) : string; overload;
function  ALDESEncryptStringCBC(const InString: string; const Key : TAlCipherKey64; Encrypt : Boolean) : string; overload;
procedure ALDESEncryptFile(const InFile, OutFile : string; const Key : TAlCipherKey64; Encrypt : Boolean); overload;
procedure ALDESEncryptFileCBC(const InFile, OutFile : string; const Key : TAlCipherKey64; Encrypt : Boolean); overload;
procedure ALDESEncryptStream(InStream, OutStream : TStream; const Key : TAlCipherKey64; Encrypt : Boolean); overload;
procedure ALDESEncryptStreamCBC(InStream, OutStream : TStream; const Key : TAlCipherKey64; Encrypt : Boolean); overload;
procedure ALDESEncryptString(const InString: string; var OutString : string; const Key : string; Encrypt : Boolean); overload;
procedure ALDESEncryptStringCBC(const InString: string; var OutString : string; const Key : string; Encrypt : Boolean); overload;
function  ALDESEncryptString(const InString: string; const Key : string; Encrypt : Boolean) : string; overload;
function  ALDESEncryptStringCBC(const InString: string; const Key : string; Encrypt : Boolean) : string; overload;
procedure ALDESEncryptFile(const InFile, OutFile : string; const Key : string; Encrypt : Boolean); overload;
procedure ALDESEncryptFileCBC(const InFile, OutFile : string; const Key : string; Encrypt : Boolean); overload;
procedure ALDESEncryptStream(InStream, OutStream : TStream; const Key : string; Encrypt : Boolean); overload;
procedure ALDESEncryptStreamCBC(InStream, OutStream : TStream; const Key : string; Encrypt : Boolean); overload;


{Triple DES}
procedure ALTripleDESEncryptString(const InString: string; var OutString : string; const Key : TALCipherKey128; Encrypt : Boolean); overload;
procedure ALTripleDESEncryptStringCBC(const InString: string; var OutString : string; const Key : TALCipherKey128; Encrypt : Boolean); overload;
function  ALTripleDESEncryptString(const InString: string; const Key : TALCipherKey128; Encrypt : Boolean) : string; overload;
function  ALTripleDESEncryptStringCBC(const InString: string; const Key : TALCipherKey128; Encrypt : Boolean) : string; overload;
procedure ALTripleDESEncryptFile(const InFile, OutFile : string; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALTripleDESEncryptFileCBC(const InFile, OutFile : string; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALTripleDESEncryptStream(InStream, OutStream : TStream; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALTripleDESEncryptStreamCBC(InStream, OutStream : TStream; const Key : TAlCipherKey128; Encrypt : Boolean); overload;
procedure ALTripleDESEncryptString(const InString: string; var OutString : string; const Key : string; Encrypt : Boolean); overload;
procedure ALTripleDESEncryptStringCBC(const InString: string; var OutString : string; const Key : string; Encrypt : Boolean); overload;
function  ALTripleDESEncryptString(const InString: string; const Key : string; Encrypt : Boolean) : string; overload;
function  ALTripleDESEncryptStringCBC(const InString: string; const Key : string; Encrypt : Boolean) : string; overload;
procedure ALTripleDESEncryptFile(const InFile, OutFile : string; const Key : string; Encrypt : Boolean); overload;
procedure ALTripleDESEncryptFileCBC(const InFile, OutFile : string; const Key : string; Encrypt : Boolean); overload;
procedure ALTripleDESEncryptStream(InStream, OutStream : TStream; const Key : string; Encrypt : Boolean); overload;
procedure ALTripleDESEncryptStreamCBC(InStream, OutStream : TStream; const Key : string; Encrypt : Boolean); overload;

{Rijndael}
procedure ALRDLEncryptString(const InString: string; var OutString : string; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptStringCBC(const InString: string; var OutString : string; const Key; KeySize : Longint; Encrypt : Boolean); overload;
function  ALRDLEncryptString(const InString: string; const Key; KeySize : Longint; Encrypt : Boolean) : string; overload;
function  ALRDLEncryptStringCBC(const InString: string; const Key; KeySize : Longint; Encrypt : Boolean) : string; overload;
procedure ALRDLEncryptFile(const InFile, OutFile : string; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptFileCBC(const InFile, OutFile : string; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptStream(InStream, OutStream : TStream; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptStreamCBC(InStream, OutStream : TStream; const Key; KeySize : Longint; Encrypt : Boolean); overload;
procedure ALRDLEncryptString(const InString: string; var OutString : string; const Key: String; Encrypt : Boolean); overload;
procedure ALRDLEncryptStringCBC(const InString: string; var OutString : string; const Key: String; Encrypt : Boolean); overload;
function  ALRDLEncryptString(const InString: string; const Key: String; Encrypt : Boolean) : string; overload;
function  ALRDLEncryptStringCBC(const InString: string; const Key: String; Encrypt : Boolean) : string; overload;
procedure ALRDLEncryptFile(const InFile, OutFile : string; const Key: String; Encrypt : Boolean); overload;
procedure ALRDLEncryptFileCBC(const InFile, OutFile : string; const Key: String; Encrypt : Boolean); overload;
procedure ALRDLEncryptStream(InStream, OutStream : TStream; const Key: String; Encrypt : Boolean); overload;
procedure ALRDLEncryptStreamCBC(InStream, OutStream : TStream; const Key: String; Encrypt : Boolean); overload;

{ high level hashing routines }
procedure ALFileHashMD5(var Digest : TAlCipherMD5Digest; const AFileName : string); overload;
procedure ALFileHashSHA1(var Digest : TAlCipherSHA1Digest; const AFileName : string); overload;
function  ALFileHashMD5(const AFileName : string): String; overload;
function  ALFileHashSHA1(const AFileName : string): String; overload;
procedure ALStreamHashMD5(var Digest : TAlCipherMD5Digest; AStream : TStream); overload;
procedure ALStreamHashSHA1(var Digest : TAlCipherSHA1Digest; AStream : TStream); overload;
function  ALStreamHashMD5(AStream : TStream): String; overload;
function  ALStreamHashSHA1(AStream : TStream): String; overload;
procedure ALStringHashELF(var Digest : LongInt; const Str : string); overload;
procedure ALStringHashMD5(var Digest : TALCipherMD5Digest; const Str : string); overload;
procedure ALStringHashMix128(var Digest : LongInt; const Str : string); overload;
procedure ALStringHashSHA1(var Digest : TALCipherSHA1Digest; const Str : string); overload;
function  ALStringHashELF(const Str : string): LongInt; overload;
function  ALStringHashMD5(const Str : string): String; overload;
function  ALStringHashMix128(const Str : string): LongInt; overload;
function  ALStringHashSHA1(const Str : string): String; overload;


implementation

uses sysutils;

const
  cAlCryptInvalidFileFormat = 'Invalid file format';

{************************************************************************************}
function ALXorEncrypt(const InString:string; StartKey,MultKey,AddKey:Integer): string;
var c : Byte;
    I:Integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    C := Byte(InString[I]) xor (StartKey shr 8);
    Result := Result + inttohex(c ,2);
    StartKey := (c + StartKey) * MultKey + AddKey;
  end;
end;

{************************************************************************************}
function ALXorDecrypt(const InString:string; StartKey,MultKey,AddKey:Integer): string;
Var C : Char;
    I:Integer;
begin
  Result := '';
  I := 1;
  While I < Length(InString) do begin
    C := Char(strtoint('$' + copy(InString,I,2)));
    Result := Result + CHAR(Byte(C) xor (StartKey shr 8));
    StartKey := (Byte(C) + StartKey) * MultKey + AddKey;
    Inc(i,2);
  end;
end;

{*****Blowfish************************************}
procedure ALBFEncryptString(const InString: string;
                            var OutString: string;
                            const Key: TALCipherKey128;
                            Encrypt : Boolean);
begin
  OutString := ALBFEncryptString(InString, Key, Encrypt);
end;

{****************************************************}
procedure ALBFEncryptStringCBC(const InString: string;
                               var OutString: string;
                               const Key: TALCipherKey128;
                               Encrypt : Boolean);
begin
  OutString := ALBFEncryptStringCBC(InString, Key, Encrypt);
end;

{**************************************************}
function ALBFEncryptString(const InString: string;
                           const Key: TALCipherKey128;
                           Encrypt : Boolean): string;
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

{*****************************************************}
function ALBFEncryptStringCBC(const InString: string;
                              const Key: TALCipherKey128;
                              Encrypt : Boolean) : string;
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

{********************************************************************************************************}
procedure AlBFEncryptFile(const InFile, OutFile : string; const Key : TALCipherKey128; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      ALBFEncryptStream(InStream, OutStream, Key, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{***********************************************************************************************************}
procedure AlBFEncryptFileCBC(const InFile, OutFile : string; const Key : TALCipherKey128; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
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

{*************************************************}
procedure ALBFEncryptString(const InString: string;
                            var OutString : string;
                            const Key: string;
                            Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptString(InString,OutString, aCipherKey128, Encrypt);
end;

{****************************************************}
procedure ALBFEncryptStringCBC(const InString: string;
                               var OutString : string;
                               const Key: string;
                               Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptStringCBC(InString, OutString, aCipherKey128, Encrypt);
end;

{*************************************************}
function  AlBFEncryptString(const InString: string;
                            const Key: string;
                            Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Result := AlBFEncryptString(InString, aCipherKey128, Encrypt);
end;

{****************************************************}
function  ALBFEncryptStringCBC(const InString: string;
                               const Key: string;
                               Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  result := ALBFEncryptStringCBC(InString, aCipherKey128, Encrypt);
end;

{*******************************************************}
procedure ALBFEncryptStream(InStream, OutStream: TStream;
                            const Key: string;
                            Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptStream(InStream, OutStream, aCipherKey128, Encrypt);
end;

{**********************************************************}
procedure ALBFEncryptStreamCBC(InStream, OutStream: TStream;
                               const Key: string;
                               Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptStreamCBC(InStream, OutStream, aCipherKey128, Encrypt);
end;

{******************************************************}
procedure ALBFEncryptFile(const InFile, OutFile: string;
                          const Key: string;
                          Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptFile(InFile, OutFile, aCipherKey128, Encrypt);
end;

{*********************************************************}
procedure ALBFEncryptFileCBC(const InFile, OutFile: string;
                             const Key: string;
                             Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptFileCBC(InFile, OutFile, aCipherKey128, Encrypt);
end;

{*****DES******************************************}
procedure ALDESEncryptString(const InString: string;
                             var OutString: string;
                             const Key: TAlCipherKey64;
                             Encrypt : Boolean);
begin
  OutString := ALDESEncryptString(InString, Key, Encrypt);
end;

{*****************************************************}
procedure ALDESEncryptStringCBC(const InString: string;
                                var OutString: string;
                                const Key: TAlCipherKey64;
                                Encrypt : Boolean);
begin
  OutString := ALDESEncryptStringCBC(InString, Key, Encrypt);
end;

{*************************************************}
function ALDESEncryptString(const InString: string;
                            const Key: TAlCipherKey64;
                            Encrypt : Boolean): string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
    InStream.Write(InString[1], Length(InString));
    InStream.Position := 0;

    if Encrypt then ALDESEncryptStream(InStream, OutStream, Key, True)
    else ALDESEncryptStream(InStream, OutStream, Key, False);
    OutStream.Position := 0;
    SetLength(Result, OutStream.Size);
    OutStream.Read(Result[1], OutStream.Size);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{****************************************************}
function ALDESEncryptStringCBC(const InString: string;
                               const Key: TAlCipherKey64;
                               Encrypt : Boolean) : string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  Try
    InStream.Write(InString[1], Length(InString));
    InStream.Position := 0;

    if Encrypt then ALDESEncryptStreamCBC(InStream, OutStream, Key, True)
    else ALDESEncryptStreamCBC(InStream, OutStream, Key, False);
    OutStream.Position := 0;
    SetLength(Result, OutStream.Size);
    OutStream.Read(Result[1], OutStream.Size);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{********************************************************************************************************}
procedure AlDESEncryptFile(const InFile, OutFile : string; const Key : TALCipherKey64; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      ALDESEncryptStream(InStream, OutStream, Key, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{***********************************************************************************************************}
procedure AlDESEncryptFileCBC(const InFile, OutFile : string; const Key : TALCipherKey64; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      ALDESEncryptStreamCBC(InStream, OutStream, Key, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{*********************************************************************************************************}
procedure AlDESEncryptStream(InStream, OutStream : TStream; const Key : TALCipherKey64; Encrypt : Boolean);
var
  I          : LongInt;
  Block      : TALCipherDESBlock;
  Context    : TALCipherDESContext;
  BlockCount : LongInt;
begin
  ALCipherInitEncryptDES(Key, Context, Encrypt);

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
    ALCipherEncryptDES(Context, Block);
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
    ALCipherEncryptDES(Context, Block);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptDES(Context, Block);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
end;

{************************************************************************************************************}
procedure AlDESEncryptStreamCBC(InStream, OutStream : TStream; const Key : TALCipherKey64; Encrypt : Boolean);
var
  I          : LongInt;
  Block      : TALCipherDESBlock;
  IV         : TALCipherDESBlock;
  Work       : TALCipherDESBlock;
  Context    : TALCipherDESContext;
  BlockCount : LongInt;
begin
  ALCipherInitEncryptDES(Key, Context, Encrypt);

  {get the number of blocks in the file}
  BlockCount := (InStream.Size div SizeOf(Block));

  if Encrypt then begin
    {set up an initialization vector (IV)}
    Block[0] := timeGetTime;
    Block[1] := timeGetTime;
    Block[2] := timeGetTime;
    Block[3] := timeGetTime;
    ALCipherEncryptDES(Context, Block);
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
      ALCipherEncryptDESCBC(Context, IV, Block);
      IV := Block;
    end else begin
      Work := Block;
      ALCipherEncryptDESCBC(Context, IV, Block);
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
    ALCipherEncryptDESCBC(Context, IV, Block);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptDESCBC(Context, IV, Block);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
end;

{**************************************************}
procedure AlDESEncryptString(const InString: string;
                             var OutString : string;
                             const Key: string;
                             Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
    aCipherKey64: TALCipherKey64;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Move(aCipherKey128, aCipherKey64, sizeof(aCipherKey64));
  AlDESEncryptString(InString,OutString, aCipherKey64, Encrypt);
end;

{*****************************************************}
procedure AlDESEncryptStringCBC(const InString: string;
                                var OutString : string;
                                const Key: string;
                                Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
    aCipherKey64: TALCipherKey64;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Move(aCipherKey128, aCipherKey64, sizeof(aCipherKey64));
  AlDESEncryptStringCBC(InString, OutString, aCipherKey64, Encrypt);
end;

{**************************************************}
function  AlDESEncryptString(const InString: string;
                             const Key: string;
                             Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
    aCipherKey64: TALCipherKey64;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Move(aCipherKey128, aCipherKey64, sizeof(aCipherKey64));
  Result := AlDESEncryptString(InString, aCipherKey64, Encrypt);
end;

{*****************************************************}
function  AlDESEncryptStringCBC(const InString: string;
                                const Key: string;
                                Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
    aCipherKey64: TALCipherKey64;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Move(aCipherKey128, aCipherKey64, sizeof(aCipherKey64));
  result := AlDESEncryptStringCBC(InString, aCipherKey64, Encrypt);
end;

{********************************************************}
procedure AlDESEncryptStream(InStream, OutStream: TStream;
                             const Key: string;
                             Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
    aCipherKey64: TALCipherKey64;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Move(aCipherKey128, aCipherKey64, sizeof(aCipherKey64));
  AlDESEncryptStream(InStream, OutStream, aCipherKey64, Encrypt);
end;

{***********************************************************}
procedure AlDESEncryptStreamCBC(InStream, OutStream: TStream;
                                const Key: string;
                                Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
    aCipherKey64: TALCipherKey64;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Move(aCipherKey128, aCipherKey64, sizeof(aCipherKey64));
  AlDESEncryptStreamCBC(InStream, OutStream, aCipherKey64, Encrypt);
end;

{*******************************************************}
procedure AlDESEncryptFile(const InFile, OutFile: string;
                           const Key: string;
                           Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
    aCipherKey64: TALCipherKey64;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Move(aCipherKey128, aCipherKey64, sizeof(aCipherKey64));
  AlDESEncryptFile(InFile, OutFile, aCipherKey64, Encrypt);
end;

{**********************************************************}
procedure AlDESEncryptFileCBC(const InFile, OutFile: string;
                              const Key: string;
                              Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
    aCipherKey64: TALCipherKey64;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Move(aCipherKey128, aCipherKey64, sizeof(aCipherKey64));
  AlDESEncryptFileCBC(InFile, OutFile, aCipherKey64, Encrypt);
end;

{*****Triple DES*****************************************}
procedure AlTripleDESEncryptString(const InString: string;
                                   var OutString: string;
                                   const Key: TALCipherKey128;
                                   Encrypt : Boolean);
begin
  OutString := AlTripleDESEncryptString(InString, Key, Encrypt);
end;

{***********************************************************}
procedure AlTripleDESEncryptStringCBC(const InString: string;
                                      var OutString: string;
                                      const Key: TALCipherKey128;
                                      Encrypt : Boolean);
begin
  OutString := AlTripleDESEncryptStringCBC(InString, Key, Encrypt);
end;

{*******************************************************}
function AlTripleDESEncryptString(const InString: string;
                                  const Key: TALCipherKey128;
                                  Encrypt : Boolean): string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
    InStream.Write(InString[1], Length(InString));
    InStream.Position := 0;

    if Encrypt then AlTripleDESEncryptStream(InStream, OutStream, Key, True)
    else AlTripleDESEncryptStream(InStream, OutStream, Key, False);
    OutStream.Position := 0;
    SetLength(Result, OutStream.Size);
    OutStream.Read(Result[1], OutStream.Size);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{**********************************************************}
function AlTripleDESEncryptStringCBC(const InString: string;
                                     const Key: TALCipherKey128;
                                     Encrypt : Boolean) : string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  Try
    InStream.Write(InString[1], Length(InString));
    InStream.Position := 0;

    if Encrypt then AlTripleDESEncryptStreamCBC(InStream, OutStream, Key, True)
    else AlTripleDESEncryptStreamCBC(InStream, OutStream, Key, False);
    OutStream.Position := 0;
    SetLength(Result, OutStream.Size);
    OutStream.Read(Result[1], OutStream.Size);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{***************************************************************************************************************}
procedure AlTripleDESEncryptFile(const InFile, OutFile : string; const Key : TALCipherKey128; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      ALTripleDESEncryptStream(InStream, OutStream, Key, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{******************************************************************************************************************}
procedure AlTripleDESEncryptFileCBC(const InFile, OutFile : string; const Key : TALCipherKey128; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      alTripleDESEncryptStreamCBC(InStream, OutStream, Key, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{****************************************************************************************************************}
procedure AlTripleDESEncryptStream(InStream, OutStream : TStream; const Key : TALCipherKey128; Encrypt : Boolean);
var
  I          : LongInt;
  Block      : TALCipherDESBlock;
  Context    : TALCipherTripleDESContext;
  BlockCount : LongInt;
begin
  ALCipherInitEncryptTripleDES(Key, Context, Encrypt);

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
    ALCipherEncryptTripleDES(Context, Block);
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
    ALCipherEncryptTripleDES(Context, Block);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptTripleDES(Context, Block);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
end;

{*******************************************************************************************************************}
procedure AlTripleDESEncryptStreamCBC(InStream, OutStream : TStream; const Key : TALCipherKey128; Encrypt : Boolean);
var
  I          : LongInt;
  Block      : TALCipherDESBlock;
  IV         : TALCipherDESBlock;
  Work       : TALCipherDESBlock;
  Context    : TALCipherTripleDESContext;
  BlockCount : LongInt;
begin
  ALCipherInitEncryptTripleDES(Key, Context, Encrypt);

  {get the number of blocks in the file}
  BlockCount := (InStream.Size div SizeOf(Block));

  if Encrypt then begin
    {set up an initialization vector (IV)}
    Block[0] := timeGetTime;
    Block[1] := timeGetTime;
    Block[2] := timeGetTime;
    Block[3] := timeGetTime;

    ALCipherEncryptTripleDES(Context, Block);
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
      ALCipherEncryptTripleDESCBC(Context, IV, Block);
      IV := Block;
    end else begin
      Work := Block;
      ALCipherEncryptTripleDESCBC(Context, IV, Block);
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
    ALCipherEncryptTripleDESCBC(Context, IV, Block);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptTripleDESCBC(Context, IV, Block);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
end;

{********************************************************}
procedure AlTripleDESEncryptString(const InString: string;
                                   var OutString : string;
                                   const Key: string;
                                   Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlTripleDESEncryptString(InString,OutString, aCipherKey128, Encrypt);
end;

{***********************************************************}
procedure AlTripleDESEncryptStringCBC(const InString: string;
                                      var OutString : string;
                                      const Key: string;
                                      Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlTripleDESEncryptStringCBC(InString, OutString, aCipherKey128, Encrypt);
end;

{********************************************************}
function  AlTripleDESEncryptString(const InString: string;
                                   const Key: string;
                                   Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Result := AlTripleDESEncryptString(InString, aCipherKey128, Encrypt);
end;

{***********************************************************}
function  AlTripleDESEncryptStringCBC(const InString: string;
                                      const Key: string;
                                      Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  result := AlTripleDESEncryptStringCBC(InString, aCipherKey128, Encrypt);
end;

{**************************************************************}
procedure AlTripleDESEncryptStream(InStream, OutStream: TStream;
                                   const Key: string;
                                   Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlTripleDESEncryptStream(InStream, OutStream, aCipherKey128, Encrypt);
end;

{*****************************************************************}
procedure AlTripleDESEncryptStreamCBC(InStream, OutStream: TStream;
                                      const Key: string;
                                      Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlTripleDESEncryptStreamCBC(InStream, OutStream, aCipherKey128, Encrypt);
end;

{*******************************************************}
procedure AlTripleDESEncryptFile(const InFile, OutFile: string;
                                 const Key: string;
                                 Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlTripleDESEncryptFile(InFile, OutFile, aCipherKey128, Encrypt);
end;

{****************************************************************}
procedure AlTripleDESEncryptFileCBC(const InFile, OutFile: string;
                                    const Key: string;
                                    Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlTripleDESEncryptFileCBC(InFile, OutFile, aCipherKey128, Encrypt);
end;

{*****Rijndael*************************************}
procedure AlRDLEncryptString(const InString: string;
                             var OutString: string;
                             const Key;
                             KeySize : Longint;
                             Encrypt : Boolean);
begin
  OutString := AlRDLEncryptString(InString, Key, KeySize, Encrypt);
end;

{****************************************************}
procedure AlRDLEncryptStringCBC(const InString: string;
                                var OutString: string;
                                const Key;
                                KeySize : Longint;
                                Encrypt : Boolean);
begin
  OutString := AlRDLEncryptStringCBC(InString, Key, KeySize, Encrypt);
end;

{**************************************************}
function AlRDLEncryptString(const InString: string;
                            const Key;
                            KeySize : Longint;
                            Encrypt : Boolean): string;
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

{*****************************************************}
function AlRDLEncryptStringCBC(const InString: string;
                               const Key;
                               KeySize : Longint;
                               Encrypt : Boolean) : string;
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

{**********************************************************************************************************}
procedure AlRDLEncryptFile(const InFile, OutFile : string; const Key; KeySize : Longint; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      ALRDLEncryptStream(InStream, OutStream, Key, KeySize, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{*************************************************************************************************************}
procedure AlRDLEncryptFileCBC(const InFile, OutFile : string; const Key; KeySize : Longint; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
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

{**************************************************}
procedure AlRDLEncryptString(const InString: string;
                             var OutString : string;
                             const Key: string;
                             Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptString(InString,OutString, aCipherKey128, 16, Encrypt);
end;

{*****************************************************}
procedure AlRDLEncryptStringCBC(const InString: string;
                                var OutString : string;
                                const Key: string;
                                Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptStringCBC(InString, OutString, aCipherKey128, 16, Encrypt);
end;

{**************************************************}
function  AlRDLEncryptString(const InString: string;
                             const Key: string;
                             Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  Result := AlRDLEncryptString(InString, aCipherKey128, 16, Encrypt);
end;

{*****************************************************}
function  AlRDLEncryptStringCBC(const InString: string;
                                const Key: string;
                                Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  result := AlRDLEncryptStringCBC(InString, aCipherKey128, 16, Encrypt);
end;

{********************************************************}
procedure AlRDLEncryptStream(InStream, OutStream: TStream;
                             const Key: string;
                             Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptStream(InStream, OutStream, aCipherKey128, 16, Encrypt);
end;

{***********************************************************}
procedure AlRDLEncryptStreamCBC(InStream, OutStream: TStream;
                                const Key: string;
                                Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptStreamCBC(InStream, OutStream, aCipherKey128, 16, Encrypt);
end;

{*******************************************************}
procedure AlRDLEncryptFile(const InFile, OutFile: string;
                           const Key: string;
                           Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptFile(InFile, OutFile, aCipherKey128, 16, Encrypt);
end;

{**********************************************************}
procedure AlRDLEncryptFileCBC(const InFile, OutFile: string;
                              const Key: string;
                              Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  ALCipherGenerateMD5Key(aCipherKey128, Key);
  AlRDLEncryptFileCBC(InFile, OutFile, aCipherKey128, 16, Encrypt);
end;

{****MD5**************************************************************************}
procedure AlFileHashMD5(var Digest : TALCipherMD5Digest; const AFileName : string);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead);
  try
    ALStreamHashMD5(Digest, FS);
  finally
    FS.Free;
  end;
end;

{*******************************************************}
function ALFileHashMD5(const AFileName : string): String;
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

{**************************************************}
function AlStreamHashMD5(AStream : TStream): String;
Var aMD5Digest: TAlCipherMD5Digest;
Begin
  AlStreamHashMD5(aMD5Digest, AStream);
  Result := AlCipherBufferToHex(aMD5Digest, SizeOf(aMD5Digest));
end;

{****SHA1***************************************************************************}
procedure AlFileHashSHA1(var Digest : TALCipherSHA1Digest; const AFileName : string);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead);
  try
    ALStreamHashSHA1(Digest, FS);
  finally
    FS.Free;
  end;
end;

{*********************************************************}
function  ALFileHashSHA1(const AFileName : string): String;
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

{***************************************************}
function AlStreamHashSHA1(AStream : TStream): string;
Var aSHA1Digest: TALCipherSHA1Digest;
Begin
  AlStreamHashSHA1(aSHA1Digest, AStream);
  Result := AlCipherBufferToHex(aSHA1Digest, SizeOf(aSHA1Digest));
end;

{******************************************************************}
procedure ALStringHashELF(var Digest : LongInt; const Str : string);
begin
  ALCipherHashELF(Digest, Str[1], Length(Str));
end;

{****************************************************}
function ALStringHashELF(const Str : string): LongInt;
Begin
  AlStringHashELF(Result, str);
end;

{*********************************************************************}
procedure ALStringHashMix128(var Digest : LongInt; const Str : string);
begin
  ALCipherHashMix128(Digest, Str[1], Length(Str));
end;

{*******************************************************}
function ALStringHashMix128(const Str : string): LongInt;
Begin
  ALStringHashMix128(Result, Str);
end;

{*****************************************************************************}
procedure ALStringHashMD5(var Digest : TALCipherMD5Digest; const Str : string);
begin
  ALCipherHashMD5(Digest, Str[1], Length(Str));
end;

{***************************************************}
function ALStringHashMD5(const Str : string): String;
Var aMD5Digest: TAlCipherMD5Digest;
Begin
  AlStringHashMD5(aMD5Digest, Str);
  Result := AlCipherBufferToHex(aMD5Digest, SizeOf(aMD5Digest));
end;

{*******************************************************************************}
procedure ALStringHashSHA1(var Digest : TALCipherSHA1Digest; const Str : string);
begin
  ALCipherHashSHA1(Digest, Str[1], Length(Str));
end;

{****************************************************}
function ALStringHashSHA1(const Str : string): String;
Var aSHA1Digest: TAlCipherSHA1Digest;
Begin
  AlStringHashSHA1(aSHA1Digest, Str);
  Result := AlCipherBufferToHex(ASHA1Digest, SizeOf(aSHA1Digest));
end;

end.
