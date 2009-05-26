{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    TurboPower Software
              Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe Crypt Functions
Version:      3.50

Description:  Simple blowFish and Xor Encrypt / Decrypt functions

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
unit ALFcnCrypt;

interface

uses MMSystem,
     Classes,
     AlCipher;

{Xor function}
function  ALXorEncrypt(const InString:string; StartKey,MultKey,AddKey:Integer): string;
function  ALXorDecrypt(const InString:string; StartKey,MultKey,AddKey:Integer): string;

{blowfish functions}
procedure ALBFEncryptString(const InString: string; var OutString : string; const Key : TALCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptStringCBC(const InString: string; var OutString : string; const Key : TALCipherKey128; Encrypt : Boolean); overload;
function  AlBFEncryptString(const InString: string; const Key : TALCipherKey128; Encrypt : Boolean) : string; overload;
function  ALBFEncryptStringCBC(const InString: string; const Key : TALCipherKey128; Encrypt : Boolean) : string; overload;
procedure ALBFEncryptStream(InStream, OutStream: TStream; const Key : TALCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptStreamCBC(InStream, OutStream: TStream; const Key : TALCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptFile(const InFile, OutFile: string; const Key : TALCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptFileCBC(const InFile, OutFile: string; const Key : TALCipherKey128; Encrypt : Boolean); overload;
procedure ALBFEncryptString(const InString: string; var OutString : string; const Key: string; Encrypt : Boolean); overload;
procedure ALBFEncryptStringCBC(const InString: string; var OutString : string; const Key: string; Encrypt : Boolean); overload;
function  AlBFEncryptString(const InString: string; const Key: string; Encrypt : Boolean) : string; overload;
function  ALBFEncryptStringCBC(const InString: string; const Key: string; Encrypt : Boolean) : string; overload;
procedure ALBFEncryptStream(InStream, OutStream: TStream; const Key: string; Encrypt : Boolean); overload;
procedure ALBFEncryptStreamCBC(InStream, OutStream: TStream; const Key: string; Encrypt : Boolean); overload;
procedure ALBFEncryptFile(const InFile, OutFile: string; const Key: string; Encrypt : Boolean); overload;
procedure ALBFEncryptFileCBC(const InFile, OutFile: string; const Key: string; Encrypt : Boolean); overload;

implementation

uses sysutils;

const cALCryptInvalidFileFormat = 'Invalid file format';

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

{*************************************************}
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

{********************************************************}
procedure ALBFEncryptStream(InStream, OutStream : TStream;
                            const Key : TALCipherKey128;
                            Encrypt : Boolean);
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
      raise EALCipherException.Create(cALCryptInvalidFileFormat);
    ALCipherEncryptBF(Context, Block, Encrypt);
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
    ALCipherEncryptBF(Context, Block, Encrypt);
    OutStream.Write(Block, SizeOf(Block));
  end
  else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise EALCipherException.Create(cAlCryptInvalidFileFormat);
    ALCipherEncryptBF(Context, Block, Encrypt);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
end;

{***********************************************************}
procedure ALBFEncryptStreamCBC(InStream, OutStream : TStream;
                               const Key : TALCipherKey128;
                               Encrypt : Boolean);
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
  end
  else begin
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
    end
    else begin
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
  end
  else begin
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

{*******************************************************}
procedure ALBFEncryptFile(const InFile, OutFile : string;
                          const Key : TALCipherKey128;
                          Encrypt : Boolean);
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

{***********************************************************}
procedure ALBFEncryptFileCBC(const InFile, OutFile : string;
                             const Key : TALCipherKey128;
                             Encrypt : Boolean);
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

{*************************************************}
procedure ALBFEncryptString(const InString: string;
                            var OutString : string;
                            const Key: string;
                            Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  GenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptString(InString,OutString, aCipherKey128, Encrypt);
end;

{****************************************************}
procedure ALBFEncryptStringCBC(const InString: string;
                               var OutString : string;
                               const Key: string;
                               Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  GenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptStringCBC(InString, OutString, aCipherKey128, Encrypt);
end;

{*************************************************}
function  AlBFEncryptString(const InString: string;
                            const Key: string;
                            Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
begin
  GenerateMD5Key(aCipherKey128, Key);
  Result := AlBFEncryptString(InString, aCipherKey128, Encrypt);
end;

{****************************************************}
function  ALBFEncryptStringCBC(const InString: string;
                               const Key: string;
                               Encrypt : Boolean) : string;
var aCipherKey128: TALCipherKey128;
begin
  GenerateMD5Key(aCipherKey128, Key);
  result := ALBFEncryptStringCBC(InString, aCipherKey128, Encrypt);
end;

{*******************************************************}
procedure ALBFEncryptStream(InStream, OutStream: TStream;
                            const Key: string;
                            Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  GenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptStream(InStream, OutStream, aCipherKey128, Encrypt);
end;

{**********************************************************}
procedure ALBFEncryptStreamCBC(InStream, OutStream: TStream;
                               const Key: string;
                               Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  GenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptStreamCBC(InStream, OutStream, aCipherKey128, Encrypt);
end;

{******************************************************}
procedure ALBFEncryptFile(const InFile, OutFile: string;
                          const Key: string;
                          Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  GenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptFile(InFile, OutFile, aCipherKey128, Encrypt);
end;

{*********************************************************}
procedure ALBFEncryptFileCBC(const InFile, OutFile: string;
                             const Key: string;
                             Encrypt : Boolean);
var aCipherKey128: TALCipherKey128;
begin
  GenerateMD5Key(aCipherKey128, Key);
  ALBFEncryptFileCBC(InFile, OutFile, aCipherKey128, Encrypt);
end;

end.
