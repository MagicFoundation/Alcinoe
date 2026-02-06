unit Alcinoe.Cipher;

interface

{$I Alcinoe.inc}

uses
  system.sysutils,
  {$IF defined(MSWINDOWS)}
  winapi.windows,
  {$ELSE}
  system.types,
  {$ENDIF}
  system.hash;


/////////////////
////// MD5 //////
/////////////////

type
  TALMD5Digest  = array [0..15] of Byte;         { 128 bits - MD5 }

procedure ALStringHashMD5(var Digest : TALMD5Digest; const Str : AnsiString); overload;
procedure ALStringHashMD5(var Digest: TALMD5Digest; const Str: String; Const encoding: Tencoding); overload;
function  ALStringHashMD5(const Str : AnsiString; const HexEncode: boolean = true): AnsiString; overload;
function  ALStringHashMD5(const Str: String; Const encoding: Tencoding): String; overload; // result will be hexencoded


//////////////////
////// SHA1 //////
//////////////////

type
  TALSHA1Digest = array [0..19] of Byte;         { 160 bits - SHA-1 }

procedure ALStringHashSHA1(var Digest: TALSHA1Digest; const Str: AnsiString); overload;
procedure ALStringHashSHA1(var Digest: TALSHA1Digest; const Str : String; Const encoding: Tencoding); overload;
function  ALStringHashSHA1(const Str: AnsiString; const HexEncode: boolean = true): AnsiString; overload;
function  ALStringHashSHA1(const Str: String; Const encoding: Tencoding): String; overload; // result will be hexencoded


//////////////////
////// SHA2 //////
//////////////////

procedure ALStringHashSHA2(var Digest: TBytes; const Str: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256); overload;
procedure ALStringHashSHA2(var Digest: Tbytes; const Str: String; Const encoding: Tencoding; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256); overload;
function  ALStringHashSHA2(const Str: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const HexEncode: boolean = true): AnsiString; overload;
function  ALStringHashSHA2(const Str: String; Const encoding: Tencoding; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): String; overload; // result will be hexencoded


/////////////////////////////
////// HMAC algorithms //////
/////////////////////////////

function  ALCalcHMACSHA1(const Str, Key : AnsiString): AnsiString;
function  ALCalcHMACMD5(const Str, Key : AnsiString): AnsiString;


//////////////////////////
////// Random bytes //////
//////////////////////////

{$IFDEF MSWINDOWS}
procedure ALRandomBytes(const Dest; const Len: Cardinal); overload;
function ALRandomBytes(const Len: Cardinal): TBytes; overload;
function ALRandomByteStr(const Len: Cardinal): ansiString;
{$ENDIF}
var ALRandom32: function(const ARange: Cardinal): cardinal;
var ALRandom64: function(const ARange: UInt64): UInt64;


///////////////////
////// Fnv1a //////
///////////////////

function ALFnv1aInt32(const str: ansiString): int32; overload; inline;
function ALFnv1aInt32(const str: String; Const encoding: Tencoding): int32; overload; inline;
function ALFnv1aInt64(const str: ansiString): Int64; overload; inline;
function ALFnv1aInt64(const str: String; Const encoding: Tencoding): Int64; overload; inline;


///////////////////////
////// Signature //////
///////////////////////

{$IF defined(MSWINDOWS)}
function ALVerifyRSA256Signature(
           const AData: AnsiString; // bytes string
           const ASignature: AnsiString; // bytes string
           const ABase64PubKeyModulus: ansiString;
           Const ABase64PubKeyExponent: ansiString): boolean;
function ALRSA256Sign(
           const AData: AnsiString; // bytes string
           const APemPrivateKey: AnsiString): ansiString; // byte string result
{$ENDIF}


////////////////////
////// OAuth2 //////
////////////////////

{$IF defined(MSWINDOWS)}
function ALGenerateGoogleOAuth2AccessToken(
           const AServiceAccountEmail: ansiString;
           const AScope: ansiString;
           const APrivateKey: ansiString): AnsiString;
{$ENDIF}


////////////////////
////// WINAPI //////
////////////////////

{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}

const
  crypt32 = 'crypt32.dll';
  bcrypt = 'bcrypt.dll';

const
  CRYPT_VERIFYCONTEXT = $F0000000;
  CRYPT_NEWKEYSET = $00000008;
  CRYPT_SILENT = $00000040;
  //-----
  PROV_RSA_FULL = $00000001;
  PROV_RSA_AES = $00000018;
  //-----
  MS_ENHANCED_PROV_A = ansiString('Microsoft Enhanced Cryptographic Provider v1.0');
  MS_ENHANCED_PROV_W = string('Microsoft Enhanced Cryptographic Provider v1.0');
  //-----
  CRYPT_STRING_BASE64HEADER = $00000000;
  CRYPT_STRING_BASE64 = $00000001;
  //-----
  PUBLICKEYBLOB = $6;
  //-----
  CUR_BLOB_VERSION = 2;
  //-----
  //https://docs.microsoft.com/en-us/windows/win32/seccrypto/alg-id
  CALG_RSA_KEYX = $0000a400;
  CALG_SHA_256 = $0000800c;
  //-----
  RSA1 = $31415352;
  //-----
  X509_ASN_ENCODING = $00000001;
  PKCS_7_ASN_ENCODING = $00010000;
  //-----
  PKCS_RSA_PRIVATE_KEY = LPCSTR(43);
  PKCS_PRIVATE_KEY_INFO = LPCSTR(44);
  PKCS_ENCRYPTED_PRIVATE_KEY_INFO = LPCSTR(45);
  //-----
  // https://docs.microsoft.com/it-it/windows/win32/seccng/cng-algorithm-identifiers
  BCRYPT_AES_ALGORITHM    = 'AES';
  BCRYPT_RNG_ALGORITHM    = 'RNG';
  BCRYPT_RSA_ALGORITHM    = 'RSA';
  BCRYPT_SHA256_ALGORITHM = 'SHA256';
  //-----
  BCRYPT_PAD_NONE  = $00000001;
  BCRYPT_PAD_PKCS1 = $00000002;
  BCRYPT_PAD_OAEP  = $00000004;
  BCRYPT_PAD_PSS   = $00000008;
  //-----
  BCRYPT_RSAPUBLIC_BLOB       = 'RSAPUBLICBLOB';
  BCRYPT_RSAPRIVATE_BLOB      = 'RSAPRIVATEBLOB';
  BCRYPT_RSAFULLPRIVATE_BLOB  = 'RSAFULLPRIVATEBLOB';
  //-----
  BCRYPT_RSAPUBLIC_MAGIC      = $31415352;  // RSA1
  BCRYPT_RSAPRIVATE_MAGIC     = $32415352;  // RSA2
  BCRYPT_RSAFULLPRIVATE_MAGIC = $33415352;  // RSA3
  //-----
  STATUS_SUCCESS            = $00000000;

type
  ALG_ID = UINT;
  HCRYPTPROV = ULONG_PTR;
  PHCRYPTPROV = ^HCRYPTPROV;
  HCRYPTKEY = ULONG_PTR;
  PHCRYPTKEY = ^HCRYPTKEY;
  HCRYPTHASH = ULONG_PTR;
  PHCRYPTHASH = ^HCRYPTHASH;

  _PUBLICKEYSTRUC = record
    bType   : BYTE;
    bVersion: BYTE;
    reserved: WORD;
    aiKeyAlg: ALG_ID;
  end;
  BLOBHEADER = _PUBLICKEYSTRUC;
  PUBLICKEYSTRUC = _PUBLICKEYSTRUC;

  _RSAPUBKEY = record
    magic : DWORD;
    bitlen: DWORD;
    pubexp: DWORD;
  end;
  RSAPUBKEY  = _RSAPUBKEY;

  PFN_CRYPT_ALLOC = function(cbSize: size_t): LPVOID; stdcall;
  PFN_CRYPT_FREE = procedure(pv: LPVOID); stdcall;

  PCRYPT_DECODE_PARA = ^CRYPT_DECODE_PARA;
  _CRYPT_DECODE_PARA = record
    cbSize: DWORD;
    pfnAlloc: PFN_CRYPT_ALLOC; // OPTIONAL
    pfnFree: PFN_CRYPT_FREE;   // OPTIONAL
  end;
  CRYPT_DECODE_PARA = _CRYPT_DECODE_PARA;

  NTSTATUS = LONG;
  BCRYPT_HANDLE = PVOID;
  BCRYPT_ALG_HANDLE = PVOID;
  BCRYPT_KEY_HANDLE = PVOID;
  BCRYPT_HASH_HANDLE = PVOID;
  BCRYPT_SECRET_HANDLE = PVOID;

  //https://docs.microsoft.com/en-us/windows/win32/seccrypto/base-provider-key-blobs
  PRIVATEKEYBLOB = record
    PublicKeyStruc: BLOBHEADER; // A PUBLICKEYSTRUC structure.
    RSAPubKey: RSAPUBKEY; // A RSAPUBKEY structure. The magic member must be set to 0x32415352. This hexadecimal value is the ASCII encoding of RSA2.
    Modulus: TBytes; // The modulus. This has a value of Prime1×Prime2 and is often known as n.
    Prime1: TBytes; // Prime number 1, often known as p.
    Prime2: TBytes; // Prime number 2, often known as q.
    Exponent1: TBytes; // Exponent 1. This has a numeric value of d mod (p – 1).
    Exponent2: TBytes; // Exponent 2. This has a numeric value of d mod (q – 1).
    Coefficient: TBytes; // Coefficient. This has a numeric value of (inverse of q) mod p.
    PrivateExponent: TBytes; // Private exponent, often known as d.
  end;

  // https://docs.microsoft.com/it-it/windows/win32/api/bcrypt/ns-bcrypt-bcrypt_rsakey_blob
  BCRYPT_RSAKEY_BLOB = packed record
    Magic: ULONG;
    BitLength: ULONG;
    CbPublicExp: ULONG;
    CbModulus: ULONG;
    CbPrime1: ULONG;
    CbPrime2: ULONG;
  end;

  BCRYPT_PKCS1_PADDING_INFO = packed record
    pszAlgId: LPCWSTR;
  end;

function CryptStringToBinaryA(
           pszString: LPCSTR;
           cchString: DWORD;
           dwFlags: DWORD;
           pbBinary: pByte;
           pcbBinary: PDWORD;
           pdwSkip: PDWORD;
           pdwFlags: PDWORD): boolean; stdcall external crypt32 delayed;
function CryptDecodeObjectEx(
           dwCertEncodingType: DWORD;
           lpszStructType: LPCSTR;
           const pbEncoded: PBYTE;
           cbEncoded: DWORD;
           dwFlags: DWORD;
           pDecodePara: PCRYPT_DECODE_PARA;
           pvStructInfo: Pointer;
           pcbStructInfo: PDWORD): BOOL; stdcall external crypt32 delayed;
function CryptAcquireContextA(
           phProv: PHCRYPTPROV;
           szContainer: LPCSTR;
           szProvider: LPCSTR;
           dwProvType: DWORD;
           dwFlags: DWORD): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function CryptReleaseContext(
           hProv: HCRYPTPROV;
           dwFlags: DWORD): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function CryptGenRandom(
           hProv: HCRYPTPROV;
           dwLen: DWORD;
           pbBuffer: PBYTE): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function CryptImportKey(
           hProv: HCRYPTPROV;
           const pbData: PBYTE;
           dwDataLen: DWORD;
           hPubKey: HCRYPTKEY;
           dwFlags: DWORD;
           phKey: PHCRYPTKEY): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function CryptDestroyKey(hKey: HCRYPTKEY): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function CryptVerifySignatureA(
           hHash: HCRYPTHASH;
           const pbSignature: PBYTE;
           dwSigLen: DWORD;
           hPubKey: HCRYPTKEY;
           szDescription: LPCSTR;
           dwFlags: DWORD): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function CryptCreateHash(
           hProv: HCRYPTPROV;
           Algid: ALG_ID;
           hKey: HCRYPTKEY;
           dwFlags: DWORD;
           phHash: PHCRYPTHASH): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function CryptHashData(
           hHash: HCRYPTHASH;
           const pbData: PBYTE;
           dwDataLen: DWORD;
           dwFlags: DWORD): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function CryptSignHashA(
           hHash: HCRYPTHASH;
           dwKeySpec: DWORD;
           szDescription: LPCSTR;
           dwFlags: DWORD;
           pbSignature: PBYTE;
           pdwSigLen: PDWORD): BOOL; stdcall external ADVAPI32 delayed; // deprecated;
function BCryptOpenAlgorithmProvider(
           out phAlgorithm: BCRYPT_ALG_HANDLE;
           pszAlgId: LPCWSTR;
           pszImplementation: LPCWSTR;
           dwFlags: ULONG): NTSTATUS; stdcall; external bcrypt delayed;
function BCryptCloseAlgorithmProvider(
           hAlgorithm: BCRYPT_ALG_HANDLE;
           dwFlags: ULONG): NTSTATUS; stdcall; external bcrypt delayed;
function BCryptImportKeyPair(
           hAlgorithm: BCRYPT_ALG_HANDLE;
           hImportKey: BCRYPT_KEY_HANDLE;
           pszBlobType: LPCWSTR;
           out phKey: BCRYPT_KEY_HANDLE;
           pbInput: PUCHAR;
           cbInput: ULONG;
           dwFlags: ULONG): NTSTATUS; stdcall; external bcrypt delayed;
function BCryptDestroyKey(hKey: BCRYPT_KEY_HANDLE): NTSTATUS; stdcall; external bcrypt delayed;
function BCryptSignHash(
           hKey: BCRYPT_KEY_HANDLE;
           pPaddingInfo: Pointer;
           pbInput: PUCHAR;
           cbInput: ULONG;
           pbOutput: PUCHAR;
           cbOutput: ULONG;
           var pcbResult: ULONG;
           dwFlags: ULONG): NTSTATUS; stdcall; external bcrypt delayed;

{$WARN SYMBOL_PLATFORM ON}
{$ENDIF MSWINDOWS}

implementation

uses
  {$IF defined(MSWINDOWS)}
  System.SysConst,
  System.DateUtils,
  Alcinoe.WinApi.Windows,
  Alcinoe.HTTP.Client.WinHTTP,
  Alcinoe.JSONDoc,
  Alcinoe.HTML,
  {$ENDIF}
  System.Classes,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.Common;


/////////////////
////// MD5 //////
/////////////////

{*************************************************************************}
procedure ALStringHashMD5(var Digest: TALMD5Digest; const Str: AnsiString);
begin
  var LMD5 := THashMD5.Create;
  LMD5.Update(pointer(Str)^, length(str));
  var LBytes := LMD5.HashAsBytes;
  ALMove(PByte(LBytes)^, Digest[0], length(LBytes)); // << LBytes can not be bigger than Digest
end;

{*****************************************************************************}
function ALStringHashMD5(const Str: String; Const encoding: Tencoding): String;
Begin
  var LMD5 := THashMD5.Create;
  LMD5.Update(encoding.GetBytes(str));
  var LBytes := LMD5.HashAsBytes;
  Result := ALBinToHexW(PByte(LBytes)^, length(LBytes));
end;

{*******************************************************************************************}
function ALStringHashMD5(const Str: AnsiString; const HexEncode: boolean = true): AnsiString;
begin
  var LMD5 := THashMD5.Create;
  LMD5.Update(pointer(Str)^, length(str));
  var LBytes := LMD5.HashAsBytes;
  if HexEncode then result := ALBinToHexA(PByte(LBytes)^, length(LBytes))
  else begin
    setlength(result, length(LBytes));
    ALMove(PByte(LBytes)^, pointer(result)^, length(LBytes));
  end;
end;

{************************************************************************************************}
procedure ALStringHashMD5(var Digest: TALMD5Digest; const Str: String; Const encoding: Tencoding);
begin
  var LMD5 := THashMD5.Create;
  LMD5.Update(encoding.GetBytes(str));
  var LBytes := LMD5.HashAsBytes;
  ALMove(PByte(LBytes)^, Digest[0], length(LBytes)); // << LBytes can not be bigger than Digest
end;


//////////////////
////// SHA1 //////
//////////////////

{***************************************************************************}
procedure ALStringHashSHA1(var Digest: TALSHA1Digest; const Str: AnsiString);
begin
  var LSHA1 := THashSHA1.Create;
  LSHA1.Update(pointer(Str)^, length(str));
  var LBytes := LSHA1.HashAsBytes;
  ALMove(PByte(LBytes)^, Digest[0], length(LBytes)); // << LBytes can not be bigger than Digest
end;

{***************************************************************************************************}
procedure ALStringHashSHA1(var Digest: TALSHA1Digest; const Str : String; Const encoding: Tencoding);
begin
  var LSHA1 := THashSHA1.Create;
  LSHA1.Update(encoding.GetBytes(str));
  var LBytes := LSHA1.HashAsBytes;
  ALMove(PByte(LBytes)^, Digest[0], length(LBytes)); // << LBytes can not be bigger than Digest
end;

{********************************************************************************************}
function ALStringHashSHA1(const Str: AnsiString; const HexEncode: boolean = true): AnsiString;
begin
  var LSHA1 := THashSHA1.Create;
  LSHA1.Update(pointer(Str)^, length(str));
  var LBytes := LSHA1.HashAsBytes;
  if HexEncode then result := ALBinToHexA(PByte(LBytes)^, length(LBytes))
  else begin
    setlength(result, length(LBytes));
    ALMove(PByte(LBytes)^, pointer(result)^, length(LBytes));
  end;
end;

{******************************************************************************}
function ALStringHashSHA1(const Str: String; Const encoding: Tencoding): String;
Begin
  var LSHA1 := THashSHA1.Create;
  LSHA1.Update(encoding.GetBytes(str));
  var LBytes := LSHA1.HashAsBytes;
  Result := ALBinToHexW(PByte(LBytes)^, length(LBytes));
end;


//////////////////
////// SHA2 //////
//////////////////

{************************************************************************************************************************************************}
procedure ALStringHashSHA2(var Digest: TBytes; const Str: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256);
begin
  var LSHA2 := THashSHA2.Create(AHashVersion);
  LSHA2.Update(pointer(Str)^, length(str));
  Digest := LSHA2.HashAsBytes;
end;

{***********************************************************************************************************************************************************************}
procedure ALStringHashSHA2(var Digest: Tbytes; const Str: String; Const encoding: Tencoding; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256);
begin
  var LSHA2 := THashSHA2.Create(AHashVersion);
  LSHA2.Update(encoding.GetBytes(str));
  Digest := LSHA2.HashAsBytes;
end;

{************************************************************************************************************************************************************************}
function ALStringHashSHA2(const Str: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const HexEncode: boolean = true): AnsiString;
begin
  var LSHA2 := THashSHA2.Create(AHashVersion);
  LSHA2.Update(pointer(Str)^, length(str));
  var LBytes := LSHA2.HashAsBytes;
  if HexEncode then result := ALBinToHexA(PByte(LBytes)^, length(LBytes))
  else begin
    setlength(result, length(LBytes));
    ALMove(PByte(LBytes)^, pointer(result)^, length(LBytes));
  end;
end;

{**********************************************************************************************************************************************************}
function ALStringHashSHA2(const Str: String; Const encoding: Tencoding; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): String;
begin
  var LSHA2 := THashSHA2.Create(AHashVersion);
  LSHA2.Update(encoding.GetBytes(str));
  var LBytes := LSHA2.HashAsBytes;
  Result := ALBinToHexW(PByte(LBytes)^, length(LBytes));
end;


/////////////////////////////
////// HMAC algorithms //////
/////////////////////////////

{***************************************************************}
function ALCalcHMACSHA1(const Str, Key : AnsiString): AnsiString;
Const BlockSize = 64; // Blocksize is 64 (bytes) when using one of the following hash functions: SHA-1, MD5, RIPEMD-128/160.[2]
begin
  var TmpKey: AnsiString := Key;
  if length(TmpKey) > BlockSize then begin
    var Digest: TALSHA1Digest;
    ALStringHashSHA1(Digest, TmpKey);
    setlength(TmpKey, length(Digest));
    for var I := 0 to high(Digest) do
      TmpKey[i+1] := AnsiChar(Digest[i]); // keys longer than blocksize are shortened
  end;
  if (length(TmpKey) < BlockSize) then begin
    var i := length(TmpKey) + 1;
    Setlength(TmpKey, BlockSize);
    while i <= length(TmpKey) do begin
      TmpKey[i] := #0; // keys shorter than blocksize are zero-padded
      inc(i);
    end;
  end;

  var o_key_pad: ansiString;
  setlength(o_key_pad, blocksize);
  for var I := 1 to blocksize do
    o_key_pad[I] := ansiChar($5C xor ord(TmpKey[i]));

  var i_key_pad: ansiString;
  setlength(i_key_pad, blocksize);
  for var I := 1 to blocksize do
    i_key_pad[I] := ansiChar($36 xor ord(TmpKey[i]));

  var Digest: TALSHA1Digest;
  ALStringHashSHA1(Digest, i_key_pad + Str);
  setlength(i_key_pad, length(Digest));
  for var I := 0 to high(Digest) do
    i_key_pad[i+1] := AnsiChar(Digest[i]);

  result := ALStringHashSHA1(o_key_pad + i_key_pad); // result := hash(o_key_pad + hash(i_key_pad + message))
end;

{**************************************************************}
function ALCalcHMACMD5(const Str, Key : AnsiString): AnsiString;
Const BlockSize = 64; // Blocksize is 64 (bytes) when using one of the following hash functions: SHA-1, MD5, RIPEMD-128/160.[2]
begin
  var TmpKey: AnsiString := Key;
  if length(TmpKey) > BlockSize then begin
    var Digest: TALMD5Digest;
    ALStringHashMD5(Digest, TmpKey);
    setlength(TmpKey, length(Digest));
    for var I := 0 to high(Digest) do
      TmpKey[i+1] := AnsiChar(Digest[i]); // keys longer than blocksize are shortened
  end;
  if (length(TmpKey) < BlockSize) then begin
    var i := length(TmpKey) + 1;
    Setlength(TmpKey, BlockSize);
    while i <= length(TmpKey) do begin
      TmpKey[i] := #0; // keys shorter than blocksize are zero-padded
      inc(i);
    end;
  end;

  var o_key_pad: ansiString;
  setlength(o_key_pad, blocksize);
  for var I := 1 to blocksize do
    o_key_pad[I] := ansiChar($5C xor ord(TmpKey[i]));

  var i_key_pad: ansiString;
  setlength(i_key_pad, blocksize);
  for var I := 1 to blocksize do
    i_key_pad[I] := ansiChar($36 xor ord(TmpKey[i]));

  var Digest: TALMD5Digest;
  ALStringHashMD5(Digest, i_key_pad + Str);
  setlength(i_key_pad, length(Digest));
  for var I := 0 to high(Digest) do
    i_key_pad[i+1] := AnsiChar(Digest[i]);

  result := ALStringHashMD5(o_key_pad + i_key_pad); // result := hash(o_key_pad + hash(i_key_pad + message))
end;


//////////////////////////
////// Random bytes //////
//////////////////////////

{$IFDEF MSWINDOWS}

{*******************************************************}
procedure ALRandomBytes(const Dest; const Len: Cardinal);
begin
  if Len = 0 then exit;
  var hProv: HCRYPTPROV;
  if (not CryptAcquireContextA(
            @hProv,
            nil,
            nil,
            PROV_RSA_FULL,
            CRYPT_VERIFYCONTEXT or CRYPT_SILENT)) then raiselastOsError;
  try
    if not CryptGenRandom(hProv,Len,@Dest) then raiselastOsError;
  finally
    if not CryptReleaseContext(hProv,0) then raiseLastOsError;
  end;
end;

{**************************************************}
function ALRandomBytes(const Len: Cardinal): TBytes;
begin
  if Len = 0 then begin
    SetLength(Result,0);
    exit;
  end;
  var hProv: HCRYPTPROV;
  if (not CryptAcquireContextA(
            @hProv,
            nil,
            nil,
            PROV_RSA_FULL,
            CRYPT_VERIFYCONTEXT or CRYPT_SILENT)) then raiselastOsError;
  try
    SetLength(Result,Len);
    if not CryptGenRandom(hProv,Len,@Result[0]) then raiselastOsError;
  finally
    if not CryptReleaseContext(hProv,0) then raiseLastOsError;
  end;
end;

{********************************************************}
function ALRandomByteStr(const Len: Cardinal): AnsiString;
begin
  if Len = 0 then exit('');
  var hProv: HCRYPTPROV;
  if (not CryptAcquireContextA(
            @hProv,
            nil,
            nil,
            PROV_RSA_FULL,
            CRYPT_VERIFYCONTEXT or CRYPT_SILENT)) then raiselastOsError;
  try
    SetLength(Result,Len);
    if not CryptGenRandom(hProv,Len,@Result[low(result)]) then raiselastOsError;
  finally
    if not CryptReleaseContext(hProv,0) then raiseLastOsError;
  end;
end;

{$ENDIF MSWINDOWS}

{$IFDEF ALCPUXASM}

{*******************************************************************************************************************}
// https://software.intel.com/en-us/articles/intel-digital-random-number-generator-drng-software-implementation-guide
function _TryRdRand32(out Value: Cardinal): Boolean;
asm
{$if defined(CPUX64)}
  .noframe
  // rdrand eax
  db   $0f
  db   $c7
  db   $f0
  jnc  @fail
  mov  [rcx],eax
{$elseif defined(CPUX86)}
  // rdrand ecx
  db   $0f
  db   $c7
  db   $f1
  jnc  @fail
  mov  [eax],ecx
{$else}
  {$Message Fatal 'TryRdRand32 not implemented for this platform'}
{$endif}
  mov  eax,1
  ret
@fail:
  xor  eax,eax
end;

{*******************************************************************************************************************}
// https://software.intel.com/en-us/articles/intel-digital-random-number-generator-drng-software-implementation-guide
{$if defined(CPUX64)}
function _TryRdRand64(out Value: UInt64): Boolean;
asm
  .noframe
  // rdrand rax
  db   $48  // REX.W = 1
  db   $0f
  db   $c7
  db   $f0
  jnc  @fail
  mov  [rcx],rax
  mov  eax,1
  ret
@fail:
  xor  eax,eax
end;
{$endif}

{***********************************************************}
function ALRandom32_RdRand(const ARange: Cardinal): cardinal;
begin
  //https://software.intel.com/content/www/us/en/develop/articles/intel-digital-random-number-generator-drng-software-implementation-guide.html
  //It is recommended that applications attempt 10 retries in a tight
  //loop in the unlikely event that the RDRAND instruction does not return a
  //random number. This number is based on a binomial probability argument: given
  //the design margins of the DRNG, the odds of ten failures in a row are
  //astronomically small and would in fact be an indication of a larger CPU issue.
  for var I := 1 to 10 do
    if _TryRdRand32(result) then
      exit(Result mod ARange);
  raise Exception.Create('RDRand failed!');
end;

{*******************************************************}
function ALRandom64_RdRand(const ARange: UInt64): UInt64;
begin
  //https://software.intel.com/content/www/us/en/develop/articles/intel-digital-random-number-generator-drng-software-implementation-guide.html
  //It is recommended that applications attempt 10 retries in a tight
  //loop in the unlikely event that the RDRAND instruction does not return a
  //random number. This number is based on a binomial probability argument: given
  //the design margins of the DRNG, the odds of ten failures in a row are
  //astronomically small and would in fact be an indication of a larger CPU issue.
  {$if defined(CPUX64)}
  for var I := 1 to 10 do
    if _TryRdRand64(result) then
      exit(Result mod ARange);
  {$ELSE}
  Result := (UInt64(ALRandom32_RdRand(ALMAXUInt)) shl 32) or ((UInt64(ALRandom32_RdRand(ALMAXUInt)) shl 32) shr 32);
  exit(result mod ARange);
  {$ENDIF}
  raise Exception.Create('RDRand failed!');
end;

{$ENDIF ALCPUXASM}

{************************************************************}
function ALRandom32_Default(const ARange: Cardinal): cardinal;
begin
  {$IFDEF MSWINDOWS}
  var LBytes := ALRandomBytes(sizeOf(result));
  move(LBytes[0],result,sizeOf(result));
  result := result mod ARange;
  {$ELSE}
  result := cardinal(Random(integer(ARange)));
  {$ENDIF}
end;

{********************************************************}
function ALRandom64_Default(const ARange: UInt64): UInt64;
begin
  {$IFDEF MSWINDOWS}
  var LBytes := ALRandomBytes(sizeOf(result));
  move(LBytes[0],result,sizeOf(result));
  result := result mod ARange;
  {$ELSE}
  Result := (UInt64(Random(ALMAXInt)) shl 32) or ((UInt64(Random(ALMAXInt)) shl 32) shr 32);
  result := result mod ARange;
  {$ENDIF}
end;


///////////////////
////// Fnv1a //////
///////////////////

{***********************}
{$Q-} {Overflow Checking}
//http://programmers.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed
//https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
//http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-param
function ALFnv1aInt64(const str: ansiString): Int64;
begin
   Result := Int64(14695981039346656037);
   for var i := low(str) to high(str) do
      Result := (Result xor Ord(str[i])) * 1099511628211;
end;
{$IF defined(ALOverflowCheckingON)}
  {$Q+} {Overflow Checking}
{$ENDIF}

{***********************}
{$Q-} {Overflow Checking}
function ALFnv1aInt64(const str: String; Const encoding: Tencoding): Int64;
begin
  var LBytes := encoding.GetBytes(str);
  Result := Int64(14695981039346656037);
  for var i := low(LBytes) to high(LBytes) do
    Result := (Result xor LBytes[i]) * 10995116282118;
end;
{$IF defined(ALOverflowCheckingON)}
  {$Q+} {Overflow Checking}
{$ENDIF}

{***********************}
{$Q-} {Overflow Checking}
function ALFnv1aInt32(const str: ansiString): int32;
begin
   Result := int32(2166136261);
   for var i := low(str) to high(str) do
      Result := (Result xor Ord(str[i])) * 16777619;
end;
{$IF defined(ALOverflowCheckingON)}
  {$Q+} {Overflow Checking}
{$ENDIF}

{***********************}
{$Q-} {Overflow Checking}
function ALFnv1aInt32(const str: String; Const encoding: Tencoding): int32;
begin
  var LBytes := encoding.GetBytes(str);
  Result := int32(2166136261);
  for var i := low(LBytes) to high(LBytes) do
    Result := (Result xor LBytes[i]) * 16777619;
end;
{$IF defined(ALOverflowCheckingON)}
  {$Q+} {Overflow Checking}
{$ENDIF}


///////////////////////
////// Signature //////
///////////////////////

{**}
type
  _TRSAKeyType = (PrivateKey, PublicKey, FullPrivate);

{**********************}
{$IF defined(MSWINDOWS)}
//taken from https://github.com/MattiaVicari/Crypt4Delphi
function _PKCS1Blob2PrivateKeyBlob(AKeyBlob: TBytes; AKeyBlobSize: DWORD): PRIVATEKEYBLOB;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyAndAdvance(const ASourceBuffer: TBytes; var ADestBuffer: Tbytes; var ACursor: DWORD; ASize: DWORD);
  begin
    setlength(ADestBuffer, ASize);
    if ASize = 0 then exit;
    if aSize + ACursor > AKeyBlobSize then
      raise Exception.Create('Mismatch between the Key blob size and Private key structure size');
    ALMove(ASourceBuffer[ACursor], ADestBuffer[0], ASize);
    Inc(ACursor, ASize);
  end;

begin
  var Cursor: DWORD := SizeOf(Result.PublicKeyStruc) + SizeOf(Result.RSAPubKey);
  ALMove(AKeyBlob[0], Result, Cursor);
  //-----
  var CbModulus: DWORD := Result.RSAPubKey.Bitlen div 8;
  var CbPrime: DWORD := Result.RSAPubKey.Bitlen div 16;
  //-----
  _CopyAndAdvance(AKeyBlob, Result.Modulus, Cursor, CbModulus);
  _CopyAndAdvance(AKeyBlob, Result.Prime1, Cursor, CbPrime);
  _CopyAndAdvance(AKeyBlob, Result.Prime2, Cursor, CbPrime);
  _CopyAndAdvance(AKeyBlob, Result.Exponent1, Cursor, CbPrime);
  _CopyAndAdvance(AKeyBlob, Result.Exponent2, Cursor, CbPrime);
  _CopyAndAdvance(AKeyBlob, Result.Coefficient, Cursor, CbPrime);
  _CopyAndAdvance(AKeyBlob, Result.PrivateExponent, Cursor, CbModulus);
  //-----
  if Cursor <> AKeyBlobSize then
    raise Exception.Create('Mismatch between the Key blob size and Private key structure size');
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
//taken from https://github.com/MattiaVicari/Crypt4Delphi
//transform a PRIVATEKEYBLOB in BCRYPT_RSAKEY_BLOB immediately followed by the key data
function _PrivateKeyBlob2BcryptRSAKeyBlobAndData(
           const APrivateKeyBlob: PRIVATEKEYBLOB;
           const AKeyType: _TRSAKeyType): TBytes;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _ReverseMemCopy(const Source; var Dest; Count: Integer);
  begin
    var S: PByte := PByte(@Source);
    var D: PByte := PByte(@Dest);
    for var I := 0 to Count - 1 do
      D[Count - 1 - I] := S[I];
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CheckAndCopyToBuffer(const ASourceBuffer: TBytes; var ACursor: DWORD);
  begin
    if (Length(ASourceBuffer) > 0) then begin
      _ReverseMemCopy(ASourceBuffer[0], Result[ACursor], Length(ASourceBuffer));
      Inc(ACursor, Length(ASourceBuffer));
    end;
  end;

begin

  var CbModulus: DWORD := (APrivateKeyBlob.RSAPubKey.Bitlen + 7) div 8;
  if CbModulus <> DWORD(Length(APrivateKeyBlob.Modulus)) then raise Exception.Create('Modulus size doesn''t match');

  var CbExp: DWORD;
  if APrivateKeyBlob.RSAPubKey.PubExp and $FF000000 > 0 then CbExp := 4
  else if APrivateKeyBlob.RSAPubKey.PubExp and $00FF0000 > 0 then CbExp := 3
  else if APrivateKeyBlob.RSAPubKey.PubExp and $0000FF00 > 0 then CbExp := 2
  else CbExp := 1;

  var RSAKeyBlob: BCRYPT_RSAKEY_BLOB;
  RSAKeyBlob.BitLength := APrivateKeyBlob.RSAPubKey.Bitlen;
  case AKeyType of
    _TRSAKeyType.PrivateKey: RSAKeyBlob.Magic :=  BCRYPT_RSAPRIVATE_MAGIC;
    _TRSAKeyType.PublicKey: RSAKeyBlob.Magic :=  BCRYPT_RSAPUBLIC_MAGIC;
    _TRSAKeyType.FullPrivate: RSAKeyBlob.Magic :=  BCRYPT_RSAFULLPRIVATE_MAGIC;
    else raise Exception.Create('Key type unknown');
  end;
  RSAKeyBlob.cbPublicExp := CbExp;
  RSAKeyBlob.cbModulus := CbModulus;
  RSAKeyBlob.cbPrime1 := Length(APrivateKeyBlob.Prime1);
  RSAKeyBlob.cbPrime2 := Length(APrivateKeyBlob.Prime2);

  var RSACursor: DWORD := 0;
  var CbResult: DWORD := SizeOf(RSAKeyBlob) + RSAKeyBlob.cbPublicExp;

  inc(CbResult, length(APrivateKeyBlob.Modulus));
  if AKeyType in [_TRSAKeyType.PrivateKey, _TRSAKeyType.FullPrivate] then begin
    inc(CbResult, length(APrivateKeyBlob.Prime1));
    inc(CbResult, length(APrivateKeyBlob.Prime2));
    if AKeyType = _TRSAKeyType.FullPrivate then begin
      inc(CbResult, length(APrivateKeyBlob.Exponent1));
      inc(CbResult, length(APrivateKeyBlob.Exponent2));
      inc(CbResult, length(APrivateKeyBlob.Coefficient));
      inc(CbResult, length(APrivateKeyBlob.PrivateExponent));
    end;
  end;

  SetLength(Result, CbResult);

  // Header information
  Move(RSAKeyBlob, Result[RSACursor], SizeOf(RSAKeyBlob));
  Inc(RSACursor, SizeOf(RSAKeyBlob));

  // Public exponent
  _ReverseMemCopy(APrivateKeyBlob.RSAPubKey.PubExp, Result[RSACursor], RSAKeyBlob.cbPublicExp);
  Inc(RSACursor, RSAKeyBlob.cbPublicExp);

  // Other components
  _CheckAndCopyToBuffer(APrivateKeyBlob.Modulus, RSACursor);
  if AKeyType in [_TRSAKeyType.PrivateKey, _TRSAKeyType.FullPrivate] then begin
    _CheckAndCopyToBuffer(APrivateKeyBlob.Prime1, RSACursor);
    _CheckAndCopyToBuffer(APrivateKeyBlob.Prime2, RSACursor);
    if AKeyType = _TRSAKeyType.FullPrivate then begin
      _CheckAndCopyToBuffer(APrivateKeyBlob.Exponent1, RSACursor);
      _CheckAndCopyToBuffer(APrivateKeyBlob.Exponent2, RSACursor);
      _CheckAndCopyToBuffer(APrivateKeyBlob.Coefficient, RSACursor);
      _CheckAndCopyToBuffer(APrivateKeyBlob.PrivateExponent, RSACursor);
    end;
  end;

  if RSACursor <> CbResult then
    raise Exception.Create('Mismatch between the size of the RSA key blob and the source key blob');

end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALVerifyRSA256Signature(
           const AData: AnsiString; // bytes string
           const ASignature: AnsiString; // bytes string
           const ABase64PubKeyModulus: ansiString;
           Const ABase64PubKeyExponent: ansiString): boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _bigEndianToLittleEndian(var AArr: TBytes);
  begin
    var J: integer := Length(AArr) - 1;
    var i: integer := low(AArr);
    while i < J do begin
      var B: Byte := AArr[i];
      AArr[i] := AArr[j];
      AArr[j] := B;
      Dec(j);
      inc(i);
    end;
  end;

begin

  //init pModulus / cbModulus
  var cbModulus: DWORD;
  if not CryptStringToBinaryA(
           PansiChar(ABase64PubKeyModulus), // pszString: LPCSTR;
           length(ABase64PubKeyModulus), // cchString: DWORD;
           CRYPT_STRING_BASE64, // dwFlags: DWORD;
           nil, // pbBinary: pByte;
           @cbModulus, // pcbBinary: PDWORD;
           nil, // pdwSkip: PDWORD;
           nil) then raiseLastOsError; // pdwFlags: PDWORD
  var pModulus: TBytes;
  setlength(pModulus, cbModulus);
  if not CryptStringToBinaryA(
           PansiChar(ABase64PubKeyModulus), // pszString: LPCSTR;
           length(ABase64PubKeyModulus), // cchString: DWORD;
           CRYPT_STRING_BASE64, // dwFlags: DWORD;
           @pModulus[0], // pbBinary: pByte;
           @cbModulus, // pcbBinary: PDWORD;
           nil, // pdwSkip: PDWORD;
           nil) then raiseLastOsError; // pdwFlags: PDWORD
  _bigEndianToLittleEndian(pModulus);

  //init pExponent / cbExponent
  var cbExponent: DWORD;
  if not CryptStringToBinaryA(
           PansiChar(ABase64PubKeyExponent), // pszString: LPCSTR;
           length(ABase64PubKeyExponent), // cchString: DWORD;
           CRYPT_STRING_BASE64, // dwFlags: DWORD;
           nil, // pbBinary: pByte;
           @cbExponent, // pcbBinary: PDWORD;
           nil, // pdwSkip: PDWORD;
           nil) then raiseLastOsError; // pdwFlags: PDWORD
  var pExponent: TBytes;
  setlength(pExponent, cbExponent);
  if not CryptStringToBinaryA(
           PansiChar(ABase64PubKeyExponent), // pszString: LPCSTR;
           length(ABase64PubKeyExponent), // cchString: DWORD;
           CRYPT_STRING_BASE64, // dwFlags: DWORD;
           @pExponent[0], // pbBinary: pByte;
           @cbExponent, // pcbBinary: PDWORD;
           nil, // pdwSkip: PDWORD;
           nil) then raiseLastOsError; // pdwFlags: PDWORD
  _bigEndianToLittleEndian(pExponent);
  var dwExponent: Dword;
  if cbExponent > sizeof(dwExponent) then
    raise Exception.CreateFmt('Wrong exponent (%s)',[ABase64PubKeyExponent]);
  dwExponent := 0;
  move(pExponent[0], dwExponent, cbExponent);

  //acquire a handle to a particular key container
  var hProv: HCRYPTPROV;
  if (not CryptAcquireContextA(
            @hProv, // phProv: PHCRYPTPROV;
            nil, // pszContainer: PAnsiChar;
            nil, // pszProvider: PAnsiChar;
            PROV_RSA_AES, // dwProvType: DWORD;
            CRYPT_VERIFYCONTEXT)) then raiselastOsError; // dwFlags: DWORD
  try

    // create the pKeyBlob
    // The data format is: PUBLICKEYSTRUC + RSAPUBKEY + key
    var cbKeyBlob: DWord := sizeof(PUBLICKEYSTRUC) + sizeof(RSAPUBKEY) + cbModulus;
    var pKeyBlob: Tbytes;
    setlength(pKeyBlob, cbKeyBlob);

    // Fill in the PUBLICKEYSTRUC
    var pPublicKey: PUBLICKEYSTRUC;
    pPublicKey.bType := PUBLICKEYBLOB;
    pPublicKey.bVersion := CUR_BLOB_VERSION;  // Always use this value.
    pPublicKey.reserved := 0;                 // Must be zero.
    pPublicKey.aiKeyAlg := CALG_RSA_KEYX;     // RSA public-key key exchange.
    Move(pPublicKey,pKeyBlob[0],sizeof(PUBLICKEYSTRUC));

    // Fill in the RSAPUBKEY
    var pRsaPubKey: RSAPUBKEY;
    pRsaPubKey.magic := RSA1;            // Public key.
    pRsaPubKey.bitlen := cbModulus * 8;  // Number of bits in the modulus.
    pRsaPubKey.pubexp := dwExponent;     // Exponent.
    Move(pRsaPubKey,pKeyBlob[sizeof(PUBLICKEYSTRUC)],sizeof(RSAPUBKEY));

    // Fill in the modulus
    Move(pModulus[0],pKeyBlob[sizeof(PUBLICKEYSTRUC)+sizeof(RSAPUBKEY)],cbModulus);

    // Now import the key.
    var hRSAKey: HCRYPTKEY;
    if not CryptImportKey(
             hProv, // hProv: HCRYPTPROV;
             @pKeyBlob[0], // const pbData: PBYTE;
             cbKeyBlob, // dwDataLen: DWORD;
             0, // hPubKey: HCRYPTKEY;
             0, // dwFlags: DWORD;
             @hRSAKey) then raiseLastOsError; // phKey: PHCRYPTKEY
    try

      //initiates the hashing of a stream of data.
      var hHash: HCRYPTHASH;
      if not CryptCreateHash(
               hProv, // hProv: HCRYPTPROV;
               CALG_SHA_256, // Algid: ALG_ID;
               0, // hKey: HCRYPTKEY;
               0, // dwFlags: DWORD;
               @hHash) then raiseLastOsError;
      try

        //adds data to a specified hash object.
        if not CryptHashData(
                 hHash, // hHash: HCRYPTHASH;
                 pbyte(AData), // const pbData: PBYTE;
                 length(AData), // dwDataLen: DWORD;
                 0) then raiseLastOsError; // dwFlags: DWORD

        //verifies the signature
        var pSignature: TBytes;
        setlength(pSignature, length(ASignature));
        Move(Pointer(ASignature)^, Pointer(pSignature)^, Length(ASignature));
        _bigEndianToLittleEndian(pSignature);
        if not CryptVerifySignatureA(
                 hHash, // hHash: HCRYPTHASH;
                 @pSignature[0], // const pbSignature: PBYTE;
                 length(pSignature), // dwSigLen: DWORD;
                 hRSAKey, // hPubKey: HCRYPTKEY;
                 nil, // const sDescription: LPCSTR;
                 0) then begin // dwFlags: DWORD)
          if HRESULT(GetLastError) = NTE_BAD_SIGNATURE then exit(False)
          else raiseLastOsError;
        end;

        //everything is ok
        Result := True;

      finally
        if not CryptDestroyHash(hHash) then raiseLastOsError;
      end;

    finally
      if not CryptDestroyKey(hRSAKey) then raiseLastOsError;
    end;

  finally
    if not CryptReleaseContext(hProv,0) then raiseLastOsError;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALRSA256Sign(
           const AData: AnsiString; // bytes string
           const APemPrivateKey: AnsiString): ansiString; // byte string result
begin

  //convert PKCS#8 to PKCS#1
  //!! this could be (surelly) done in a much efficient way !!
  //https://tls.mbed.org/kb/cryptography/asn1-key-structures-in-der-and-pem
  //https://stackoverflow.com/questions/64519072/how-to-import-a-pkcs8-with-cryptoapi/64520116#64520116
  //https://docs.microsoft.com/en-us/windows/win32/seccrypto/constants-for-cryptencodeobject-and-cryptdecodeobject
  if ALPosA('-----BEGIN PRIVATE KEY-----', APemPrivateKey) = 1 then begin

    var P1: Integer := ALPosA('-----',APemPrivateKey);
    if P1 <= 0 then raiseLastOsError;
    inc(P1,5{length('-----')});
    P1 := ALPosA('-----',APemPrivateKey, P1);
    if P1 <= 0 then raiseLastOsError;
    inc(P1,5{length('-----')});

    var P2: integer := ALPosA('-----',APemPrivateKey, P1);
    if P2 <= 0 then raiseLastOsError;

    var S1: ansiString := ALCopyStr(APemPrivateKey, P1, P2-P1);
    S1 := ALBase64DecodeString(S1);
    Delete(S1,1,26); // << !! on the pen I tested it's was 26 I m absolutely not sure it's will be always the case !!
    S1 := ALBase64EncodeString(S1);
    result := ALRSA256Sign(
                AData,
                '-----BEGIN RSA PRIVATE KEY-----' +
                S1 +
                '-----END RSA PRIVATE KEY-----');
    exit;

  end;

  //init PrivKey
  var cbPrivKey: DWORD;
  if not CryptStringToBinaryA(
           PansiChar(APemPrivateKey), // pszString: LPCSTR;
           length(APemPrivateKey), // cchString: DWORD;
           CRYPT_STRING_BASE64HEADER, // dwFlags: DWORD;
           nil, // pbBinary: pByte;
           @cbPrivKey, // pcbBinary: PDWORD;
           nil, // pdwSkip: PDWORD;
           nil) then raiseLastOsError; // pdwFlags: PDWORD
  var PrivKey: TBytes;
  setlength(PrivKey, cbPrivKey);
  if not CryptStringToBinaryA(
           PansiChar(APemPrivateKey), // pszString: LPCSTR;
           length(APemPrivateKey), // cchString: DWORD;
           CRYPT_STRING_BASE64HEADER, // dwFlags: DWORD;
           @PrivKey[0], // pbBinary: pByte;
           @cbPrivKey, // pcbBinary: PDWORD;
           nil, // pdwSkip: PDWORD;
           nil) then raiseLastOsError; // pdwFlags: PDWORD

  //init KeyBlob
  var cbKeyBlob: DWord;
  if not CryptDecodeObjectEx(
           X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, // dwCertEncodingType: DWORD;
           PKCS_RSA_PRIVATE_KEY, // lpszStructType: LPCSTR;
           @PrivKey[0], // const pbEncoded: PBYTE;
           cbPrivKey, // cbEncoded: DWORD;
           0, // dwFlags: DWORD;
           nil, // pDecodePara: PCRYPT_DECODE_PARA;
           nil, // pvStructInfo: Pointer;
           @cbKeyBlob) then raiseLastOsError; // pcbStructInfo: PDWORD
  var KeyBlob: Tbytes;
  setlength(KeyBlob, cbKeyBlob);
  if not CryptDecodeObjectEx(
           X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, // dwCertEncodingType: DWORD;
           PKCS_RSA_PRIVATE_KEY, // lpszStructType: LPCSTR;
           @PrivKey[0], // const pbEncoded: PBYTE;
           cbPrivKey, // cbEncoded: DWORD;
           0, // dwFlags: DWORD;
           nil, // pDecodePara: PCRYPT_DECODE_PARA;
           @KeyBlob[0], // pvStructInfo: Pointer;
           @cbKeyBlob) then raiseLastOsError; // pcbStructInfo: PDWORD

  //init RSAKeyBlobAndData
  var RSAKeyBlobAndData: TBytes;
  RSAKeyBlobAndData := _PrivateKeyBlob2BcryptRSAKeyBlobAndData(
                         _PKCS1Blob2PrivateKeyBlob(KeyBlob, cbKeyBlob),
                         _TRSAKeyType.PrivateKey);

  //loads and initializes a CNG provider
  var Algorithm: BCRYPT_ALG_HANDLE;
  ALCheckWinApiNTStatus(
    'BCryptOpenAlgorithmProvider',
    BCryptOpenAlgorithmProvider(
      Algorithm, // phAlgorithm: BCRYPT_ALG_HANDLE;
      BCRYPT_RSA_ALGORITHM, // pszAlgId: LPCWSTR;
      nil, // pszImplementation: LPCWSTR;
      0)); // dwFlags: ULONG

  try

    var KeyHandle: BCRYPT_KEY_HANDLE;
    ALCheckWinApiNTStatus(
      'BCryptImportKeyPair',
      BCryptImportKeyPair(
        Algorithm, // hAlgorithm: BCRYPT_ALG_HANDLE;
        nil, // hImportKey: BCRYPT_KEY_HANDLE;
        BCRYPT_RSAPRIVATE_BLOB, // pszBlobType: LPCWSTR;
        KeyHandle, // out phKey: BCRYPT_KEY_HANDLE;
        @RSAKeyBlobAndData[0], // pbInput: PUCHAR;
        length(RSAKeyBlobAndData), // cbInput: ULONG;
        0)); // dwFlags: ULONG
    try

      //init HashData
      var HashData: AnsiString;
      HashData := ALStringHashSHA2(AData, THashSHA2.TSHA2Version.SHA256, false{aHexEncode});
      var PaddingInfo: BCRYPT_PKCS1_PADDING_INFO;
      PaddingInfo.pszAlgId := BCRYPT_SHA256_ALGORITHM;

      //sign the data
      var cbResult: Dword;
      ALCheckWinApiNTStatus(
        'BCryptSignHash',
        BCryptSignHash(
          KeyHandle, // hKey: BCRYPT_KEY_HANDLE;
          @PaddingInfo, // pPaddingInfo: Pointer;
          PUchar(HashData), // pbInput: PUCHAR;
          Length(HashData), // cbInput: ULONG;
          nil, // pbOutput: PUCHAR;
          0, // cbOutput: ULONG;
          cbResult, // var pcbResult: ULONG;
          BCRYPT_PAD_PKCS1)); // dwFlags: ULONG
      SetLength(result, cbResult);
      ALCheckWinApiNTStatus(
        'BCryptSignHash',
        BCryptSignHash(
          KeyHandle, // hKey: BCRYPT_KEY_HANDLE;
          @PaddingInfo, // pPaddingInfo: Pointer;
          PUchar(HashData), // pbInput: PUCHAR;
          Length(HashData), // cbInput: ULONG;
          @result[low(result)], // pbOutput: PUCHAR;
          length(result), // cbOutput: ULONG;
          cbResult, // var pcbResult: ULONG;
          BCRYPT_PAD_PKCS1)); // dwFlags: ULONG

    finally
      ALCheckWinApiNTStatus(
        'BCryptDestroyKey',
        BCryptDestroyKey(KeyHandle)); // hKey: BCRYPT_KEY_HANDLE
    end;

  finally
    ALCheckWinApiNTStatus(
      'BCryptCloseAlgorithmProvider',
      BCryptCloseAlgorithmProvider(
        Algorithm, // hAlgorithm: BCRYPT_ALG_HANDLE;
        0)); // dwFlags: ULONG
  end;

end;
{$ENDIF}


////////////////////
////// OAuth2 //////
////////////////////

{**********************}
{$IF defined(MSWINDOWS)}
var
  _GoogleOAuth2AccessTokens: TALNVStringListA;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALGenerateGoogleOAuth2AccessToken(
           const AServiceAccountEmail: ansiString;
           const AScope: ansiString;
           const APrivateKey: ansiString): AnsiString;
begin

  //lock the access
  ALMonitorEnter(_GoogleOAuth2AccessTokens{$IF defined(DEBUG)}, 'ALGenerateGoogleOAuth2AccessToken'{$ENDIF});
  Try

    var LTokensListKey := AServiceAccountEmail + #30{record separator} + AScope;
    Var I := _GoogleOAuth2AccessTokens.IndexOfName(LTokensListKey);
    if I >= 0 then begin
      if UnixToDateTime({$IF defined(WIN64)}int64{$ELSE}integer{$ENDIF}(_GoogleOAuth2AccessTokens.Objects[i])) > ALUTCNow then begin
        result := _GoogleOAuth2AccessTokens.ValueFromIndex[i];
        Exit;
      end;
      _GoogleOAuth2AccessTokens.Delete(i);
    end;

    //Using OAuth 2.0 for Server to Server Applications
    //https://developers.google.com/identity/protocols/oauth2/service-account#httprest

    //Forming the JWT header
    var LJWT := ansiString('eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9'); // Base64url representation of {"alg":"RS256","typ":"JWT"}

    //Forming the JWT claim set
    var LNow := ALUtcNow;
    var LJWTclaim := '{'+
                       '"iss":"'+ALJavascriptEncode(AServiceAccountEmail)+'",'+ // The email address of the service account
                       '"scope":"'+ALJavascriptEncode(AScope)+'",'+ // A space-delimited list of the permissions that the application requests.
                       '"aud":"https://oauth2.googleapis.com/token",'+ // A descriptor of the intended target of the assertion. When making an access token request this value is always https://oauth2.googleapis.com/token.
                       '"exp":'+ALIntToStrA(DateTimeToUnix(IncHour(LNow,1)))+','+ // The expiration time of the assertion, specified as seconds since 00:00:00 UTC, January 1, 1970. This value has a maximum of 1 hour after the issued time.
                       '"iat":'+ALIntToStrA(DateTimeToUnix(LNow))+ // The time the assertion was issued, specified as seconds since 00:00:00 UTC, January 1, 1970.
                     '}';
    LJWT := LJWT + '.' + ALURLBase64EncodeString(LJWTclaim);

    //Computing the signature
    LJWT := LJWT + '.' + ALURLBase64EncodeString(ALRSA256Sign(LJWT,APrivateKey));

    //Making the access token request
    var LWinHttpClient := TAlWinHTTPClient.Create;
    var LRequestFields := TALNVStringListA.Create;
    var LJsonDoc := TALJSONDocumentA.Create;
    try

      //init the aWinHttpClient
      LWinHttpClient.ConnectTimeout := 60000;
      LWinHttpClient.SendTimeout := 60000;
      LWinHttpClient.ReceiveTimeout := 60000;

      //init LRequestFields
      LRequestFields.AddNameValue('grant_type', 'urn:ietf:params:oauth:grant-type:jwt-bearer');
      LRequestFields.AddNameValue('assertion', LJWT);

      //Renew the token
      var LHTTPClientResponse := LWinHttpClient.PostFormUrlEncoded('https://oauth2.googleapis.com/token', LRequestFields);
      Try

        if LHTTPClientResponse.StatusCode <> 200 then
          raise Exception.CreateFmt('Google OAuth2 token request failed: %d', [LHTTPClientResponse.StatusCode]);

        //Handling the response
        //{
        //  "access_token": "1/8xbJqaOZXSUZbHLl5EOtu1pxz3fmmetKx9W8CV4t79M",
        //  "scope": "https://www.googleapis.com/auth/prediction"
        //  "token_type": "Bearer",
        //  "expires_in": 3600
        //}
        LJsonDoc.LoadFromJSONString(LHTTPClientResponse.BodyString);
        Result := LJsonDoc.GetChildNodeValueText('access_token', '');
        if result = '' then raise Exception.Create('Error 8CBF4FB7-7878-4225-A26D-14369A49081A');

        //update _GoogleOAuth2AccessTokens
        _GoogleOAuth2AccessTokens.AddNameValueObject(
          LTokensListKey,
          result,
          pointer(
            {$IF defined(WIN64)}int64{$ELSE}integer{$ENDIF}(
              DateTimeToUnix(
                IncSecond(ALUtcNow, LJsonDoc.GetChildNodeValueInt32('expires_in', 0) div 2)))));

      finally
        ALFreeAndNil(LHTTPClientResponse);
      end;

    finally
      alfreeAndNil(LJsonDoc);
      alFreeAndNil(LWinHttpClient);
      alFreeAndNil(LRequestFields);
    end;

  finally
    ALMonitorExit(_GoogleOAuth2AccessTokens{$IF defined(DEBUG)}, 'ALGenerateGoogleOAuth2AccessToken'{$ENDIF});
  end;

end;
{$ENDIF}


initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.Cipher','initialization');
  {$ENDIF}

  randomize;
  ALRandom32 := ALRandom32_Default;
  ALRandom64 := ALRandom64_Default;

  {$IFDEF ALCPUXASM}

  //https://en.wikipedia.org/wiki/CPUID#EAX.3D1:_Processor_Info_and_Feature_Bits
  //The processor info and feature flags are manufacturer specific but usually the
  //Intel values are used by other manufacturers for the sake of compatibility.
  //Bit 00 = $00000001
  //Bit 01 = $00000002
  //Bit 02 = $00000004
  //Bit 03 = $00000008
  //Bit 04 = $00000010
  //Bit 05 = $00000020
  //Bit 06 = $00000040
  //Bit 07 = $00000080
  //Bit 08 = $00000100
  //Bit 09 = $00000200
  //Bit 10 = $00000400
  //Bit 11 = $00000800
  //Bit 12 = $00001000
  //Bit 13 = $00002000
  //Bit 14 = $00004000
  //Bit 15 = $00008000
  //Bit 16 = $00010000
  //Bit 17 = $00020000
  //Bit 18 = $00040000
  //Bit 19 = $00080000
  //Bit 20 = $00100000
  //Bit 21 = $00200000
  //Bit 22 = $00400000
  //Bit 23 = $00800000
  //Bit 24 = $01000000
  //Bit 25 = $02000000
  //Bit 26 = $04000000
  //Bit 27 = $08000000
  //Bit 28 = $10000000
  //Bit 29 = $20000000
  //Bit 30 = $40000000
  //Bit 31 = $80000000
  {$WARN SYMBOL_PLATFORM OFF}
  if (CPUIDTable[1].ECX and $40000000{Bit 30}) <> 0 then begin
    ALRandom32 := ALRandom32_RdRand;
    ALRandom64 := ALRandom64_RdRand;
    try
      if (ALRandom32_RdRand(ALMaxUint) = ALRandom32_RdRand(ALMaxUint)) or             // most probably a RDRAND bug,
         (ALRandom64_RdRand(ALMaxUint64) = ALRandom64_RdRand(ALMaxUint64)) then begin // e.g. on AMD Rizen 3000
        ALRandom32 := ALRandom32_Default;
        ALRandom64 := ALRandom64_Default;
      end;
    except
      // may trigger an illegal instruction exception on some Ivy Bridge
      ALRandom32 := ALRandom32_Default;
      ALRandom64 := ALRandom64_Default;
    end;
  end;
  {$WARN SYMBOL_PLATFORM ON}

  {$ENDIF ALCPUXASM}

  {$IF defined(MSWINDOWS)}
  _GoogleOAuth2AccessTokens := TALNVStringListA.Create;
  _GoogleOAuth2AccessTokens.NameValueSeparator := #31; // Unit separator
  _GoogleOAuth2AccessTokens.Duplicates := DupError;
  _GoogleOAuth2AccessTokens.Sorted := True;
  {$ENDIF}

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.Cipher','finalization');
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  AlFreeAndNil(_GoogleOAuth2AccessTokens);
  {$ENDIF}

end.